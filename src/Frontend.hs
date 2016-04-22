{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module Frontend
where

import Control.Exception (assert)
import Control.Monad.Trans.Except
import Data.List (partition)
import Lucid hiding (href_)
import Network.HTTP.Types
import Network.Wai
    ( Application, Middleware, Response
    , responseStatus, responseHeaders, responseBuilder, queryString, requestHeaders
    )
import Network.Wai.Handler.Warp as Warp (Settings, runSettings, setHost, setPort, defaultSettings)
import Network.Wai.Application.Static
    ( StaticSettings
    , ssRedirectToIndex, ssAddTrailingSlash, ssGetMimeType, defaultFileServerSettings, staticApp
    )
import Servant
import System.FilePath (addTrailingPathSeparator)

import qualified Data.ByteString.Builder as Builder

import Thentos.Prelude
import Thentos.Types (ThentosSessionToken)
import Thentos.Frontend.State (serveFAction)

import Action (ActionM, UserState, ActionEnv(..), logout)
import Action.Implementation (Action, mkRunAction)
import Arbitrary (sampleEventLog)
import Config
import Data.UriPath
import EventLog
import Frontend.Core
import Frontend.Page as Page
import Frontend.Testing
import Persistent
import Persistent.Api
import Types

import qualified Action
import qualified Backend
import qualified Frontend.Path as U


-- * driver

extendClearanceOnSessionToken :: Applicative m => ThentosSessionToken -> m ()
extendClearanceOnSessionToken _ = pure () -- FIXME

-- | Call 'runFrontend'' with the persitence implementation chosen in the config.
runFrontend :: Config -> IO ()
runFrontend cfg = withPersist cfg (runFrontend' cfg)

-- | Open a warp listener that serves the aula 'Application'.  (No content is created; on users are
-- logged in.)
runFrontend' :: Config -> RunPersist -> IO ()
runFrontend' cfg rp = do
    let runAction :: Action :~> ExceptT ServantErr IO
        runAction = mkRunAction (ActionEnv rp cfg)

        aulaTopProxy = Proxy :: Proxy AulaTop
        stateProxy   = Proxy :: Proxy UserState

    app <- serveFAction (Proxy :: Proxy AulaActions) stateProxy extendClearanceOnSessionToken
             runAction aulaActions

    let settings :: Warp.Settings
        settings = setHost (fromString $ cfg ^. listenerInterface)
                 . setPort (cfg ^. listenerPort)
                 $ Warp.defaultSettings

    runSettings settings
        . createPageSamples
        . catch404
        . serve aulaTopProxy $ aulaTop cfg app


-- * routing tables

type AulaTop
    =  "samples" :> Raw
  :<|> "static"  :> Raw
  :<|> GetH (Frame ())  -- FIXME: give this a void page type for path magic.
  :<|> Raw

aulaTop :: Config -> Application -> Server AulaTop
aulaTop cfg app =
       (\req cont -> getSamplesPath >>= \path ->
          waiServeDirectory path req cont)
  :<|> waiServeDirectory (cfg ^. htmlStatic)
  :<|> (redirect . absoluteUriPath . relPath $ U.ListSpaces)
  :<|> app
  where
    waiServeDirectory :: FilePath -> Application
    waiServeDirectory =
      staticApp . aulaTweakStaticSettings . defaultFileServerSettings .
        addTrailingPathSeparator

    aulaTweakStaticSettings :: StaticSettings -> StaticSettings
    aulaTweakStaticSettings s = s
      { ssAddTrailingSlash = True
      , ssGetMimeType = \file -> do
          mime <- ssGetMimeType s file
          -- wai's guess of the mime type is not good enough; it doesn't
          -- report character encoding. So we tweak it here manually.
          let tweakedMime "text/html" = "text/html;charset=utf8"
              tweakedMime m = m
          return $! tweakedMime mime
      , ssRedirectToIndex = True
      }


type AulaActions =
       AulaMain
  :<|> "api" :> Backend.Api
  :<|> "testing" :> AulaTesting

aulaActions :: (GenArbitrary m, ActionM m) => ServerT AulaActions m
aulaActions =
       aulaMain
  :<|> Backend.api
  :<|> aulaTesting


type AulaMain =
       -- view all spaces
       "space" :> GetH (Frame PageRoomsOverview)

       -- enter one space
  :<|> IdeaSpace ::> AulaSpace

       -- view all users
  :<|> "user" :> GetH (Frame (PageShow [User]))

       -- enter user profile
  :<|> User ::> AulaUser
  :<|> "user" :> "profile"  :> FormHandler EditUserProfile
  :<|> "user" :> "settings" :> FormHandler PageUserSettings

       -- enter admin api
  :<|> "admin" :> AulaAdmin

       -- delegation network
  :<|> "delegation" :> "edit" :> FormHandler PageDelegateVote
  :<|> "delegation" :> "view" :> GetH (Frame PageDelegationNetwork)

       -- static content
  :<|> "imprint" :> GetH (Frame PageStaticImprint)
  :<|> "terms" :> GetH (Frame PageStaticTermsOfUse)

       -- login / logout
  :<|> "login" :> FormHandler PageHomeWithLoginPrompt
  :<|> "logout" :> GetH (Frame ())  -- FIXME: give this a void page type for path magic.


aulaMain :: ActionM m => ServerT AulaMain m
aulaMain =
       makeFrame Page.viewRooms
  :<|> aulaSpace

  :<|> makeFrame (PageShow <$> Action.query getUsers)
  :<|> aulaUser
  :<|> form Page.editUserProfile
  :<|> form Page.userSettings
  :<|> aulaAdmin

  :<|> error "api not implemented: \"delegation\" :> \"edit\" :> FormHandler ()"
  :<|> makeFrame Page.viewDelegationNetwork

  :<|> makeFrame (pure PageStaticImprint)
  :<|> makeFrame (pure PageStaticTermsOfUse)

  :<|> form Page.login
  :<|> (logout >> (redirect . absoluteUriPath . relPath $ U.Login))

type CommentApi
       -- reply on a comment
    = "reply" :> FormHandler CommentIdea
       -- vote on a comment
  :<|> UpDown ::> PostH
       -- vote on a reply of a comment
  :<|> Reply ::> UpDown ::> PostH
       -- delete a comment
  :<|> "delete" :> PostH
       -- delete a comment reply
  :<|> Reply ::> "delete" :> PostH
       -- report a comment
  :<|> "report" :> PostH
       -- report a comment reply
  :<|> Reply ::> "report" :> PostH

commentApi :: ActionM m => IdeaLocation -> AUID Idea -> AUID Comment -> ServerT CommentApi m
commentApi loc iid cid
    =  form (Page.replyCommentIdea   loc iid cid)
  :<|> Action.voteIdeaComment        loc iid cid
  :<|> Action.voteIdeaCommentReply   loc iid cid
  :<|> Action.deleteIdeaComment      loc iid cid
  :<|> Action.deleteIdeaCommentReply loc iid cid
  :<|> Action.reportIdeaComment      loc iid cid
  :<|> Action.reportIdeaCommentReply loc iid cid

type IdeaApi
       -- view idea details (applies to both wild ideas and ideas in topics)
    =  Idea ::> "view" :> GetH (Frame ViewIdea)
       -- edit idea (applies to both wild ideas and ideas in topics)
  :<|> Idea ::> "edit" :> FormHandler Page.EditIdea
       -- `like' on an idea
  :<|> Idea ::> "like" :> PostH
       -- vote on an idea
  :<|> Idea ::> IdeaVoteValue ::> PostH
       -- comment on an idea
  :<|> Idea ::> "comment" :> FormHandler CommentIdea
       -- API specific to one comment
  :<|> Idea ::> Comment ::> CommentApi
       -- jury an idea
  :<|> Idea ::> IdeaJuryResultType ::> FormHandler JudgeIdea
       -- create wild idea
  :<|> "idea" :> "create" :> FormHandler CreateIdea

type TopicApi =
       -- browse topics in an idea space
       "topic" :> GetH (Frame PageIdeasInDiscussion)
  :<|> Topic ::> IdeaApi
       -- view topic details (tabs "Alle Ideen", ..., "Beauftragte Stimmen")

       -- view topic details (tabs "Alle Ideen", "Beauftragte Stimmen")
  :<|> Topic ::> "ideas"              :> IdeasFilterApi :> IdeasSortApi :> GetH (Frame ViewTopic)
  :<|> Topic ::> "ideas" :> "all"     :> IdeasFilterApi :> IdeasSortApi :> GetH (Frame ViewTopic)
  :<|> Topic ::> "ideas" :> "voting"  :> IdeasFilterApi :> IdeasSortApi :> GetH (Frame ViewTopic)
  :<|> Topic ::> "ideas" :> "winning" :> IdeasFilterApi :> IdeasSortApi :> GetH (Frame ViewTopic)
  :<|> Topic ::> "delegations"        :> GetH (Frame ViewTopic)

       -- create new topic
  :<|> "topic" :> "create"     :> FormHandler CreateTopic
  :<|> Topic  ::> "idea"       :> "move"   :> FormHandler Page.EditTopic
  :<|> Topic  ::> "delegation" :> "create" :> FormHandler PageDelegateVote

type AulaSpace
    =  IdeaApi
       -- browse wild ideas in an idea space
  :<|> "ideas" :> IdeasFilterApi :> IdeasSortApi :> GetH (Frame PageIdeasOverview)
  :<|> TopicApi

ideaApi :: ActionM m => IdeaLocation -> ServerT IdeaApi m
ideaApi loc
    =  makeFrame . Page.viewIdea
  :<|> form . Page.editIdea
  :<|> Action.likeIdea
  :<|> Action.voteIdea
  :<|> (form . Page.commentIdea loc)
  :<|> commentApi loc
  :<|> app2 form Page.judgeIdea
  :<|> form (Page.createIdea loc)

topicApi :: ActionM m => IdeaSpace -> ServerT TopicApi m
topicApi space
    =  makeFrame (Page.viewTopics space)
  :<|> ideaApi . IdeaLocationTopic space

  :<|> viewTopicTab TabAllIdeas  -- FIXME: if two paths have the same handler, one of them should be a redirect!
  :<|> viewTopicTab TabAllIdeas
  :<|> viewTopicTab TabVotingIdeas
  :<|> viewTopicTab TabWinningIdeas
  :<|> makeFrame . Page.viewTopic TabDelegation

  :<|> form (Page.createTopic space)
  :<|> form . Page.editTopic
  :<|> error "api not implemented: topic/:topic/delegation/create"
  where
    viewTopicTab tab tid qf qs = makeFrame $ Page.viewTopic (tab (IdeasQuery qf qs)) tid

aulaSpace :: ActionM m => IdeaSpace -> ServerT AulaSpace m
aulaSpace space
    =  ideaApi (IdeaLocationSpace space)
  :<|> app2 (makeFrame . Page.viewIdeas space) IdeasQuery
  :<|> topicApi space

type AulaUser =
       "ideas"       :> GetH (Frame PageUserProfileCreatedIdeas)
  :<|> "delegations" :> GetH (Frame PageUserProfileDelegatedVotes)

aulaUser :: ActionM m => AUID User -> ServerT AulaUser m
aulaUser user =
       makeFrame (Page.createdIdeas   user)
  :<|> makeFrame (Page.delegatedVotes user)


type AulaAdmin =
       -- durations
       "duration" :> FormHandler PageAdminSettingsDurations
       -- quorum
  :<|> "quorum" :> FormHandler PageAdminSettingsQuorum
       -- groups and permissions
  :<|> "users" :> GetH (Frame AdminViewUsers)
  :<|> "user" :> "create" :> FormHandler AdminCreateUser
  :<|> "classes" :> GetH (Frame AdminViewClasses)
  :<|> "class" :> "create" :> FormHandler AdminCreateClass
  :<|> User ::> "edit" :> FormHandler AdminEditUser
  :<|> SchoolClass ::> "edit" :> GetH (Frame AdminEditClass)
  :<|> User ::> "delete" :> FormHandler AdminDeleteUser
       -- event log
  :<|> "event"  :> FormHandler PageAdminSettingsEventsProtocol
  :<|> "downloads" :> "passwords" :> Capture "schoolclass" SchoolClass :> Get '[CSV] (CsvHeaders InitialPasswordsCsv)
  :<|> "downloads" :> "events" :> Get '[CSV] (CsvHeaders EventLog)
  :<|> "downloads" :> "events" :> Capture "space" IdeaSpace :> Get '[CSV] (CsvHeaders EventLog)


aulaAdmin :: ActionM m => ServerT AulaAdmin m
aulaAdmin =
       form Page.adminDurations
  :<|> form Page.adminQuorum
  :<|> makeFrame Page.adminViewUsers
  :<|> form Page.adminCreateUser
  :<|> makeFrame Page.adminViewClasses
  :<|> form Page.adminCreateClass
  :<|> form . Page.adminEditUser
  :<|> makeFrame . Page.adminEditClass
  :<|> form . Page.adminDeleteUser
  :<|> form Page.adminEventsProtocol
  :<|> Page.adminInitialPasswordsCsv
  :<|> adminEventLogCsv Nothing
  :<|> adminEventLogCsv . Just  -- FIXME: with QueryParam, we could melt these two routes into one.
                                -- this isn't a hard task, but we need to extend 'UriPath' type.

-- | FIXME: this should be in "Frontend.Page.Admin", but that would trigger a cyclical import
-- condition as long as we pull data from Arbitrary rather than from the actual events.
adminEventLogCsv :: ActionM m => Maybe IdeaSpace -> m (CsvHeaders EventLog)
adminEventLogCsv mspc = csvHeaders ("EventLog " <> maybe "alle Ideenräume" showIdeaSpaceUI mspc) .
    filterEventLog mspc <$> (viewConfig >>= pure . sampleEventLog)


catch404 :: Middleware
catch404 app req cont = app req $ \resp -> cont $ f resp
  where
    f :: Response -> Response
    f resp = if statusCode status /= 404
        then resp
        else responseBuilder status headers builder
      where
        status  = responseStatus resp
        headers = responseHeaders resp
        builder = Builder.byteString . cs . renderText . toHtml $ PublicFrame Page404


-- | If query contains @create_page_sample=true@, set header @Accept: text/plain@.  This provides a
-- way to extract page samples to feed to @src/RenderHtml.hs@.
createPageSamples :: Middleware
createPageSamples app req = app req'
  where
    req' = case partition (== ("create_page_sample", Just "true")) $ queryString req of
        ([], _)      -> req
        ([_], query) -> req { queryString = query
                            , requestHeaders = ("Accept", "text/plain") : requestHeaders req
                            }
        bad -> assert False $ error ("createPageSamples: impossible: " <> show bad)
