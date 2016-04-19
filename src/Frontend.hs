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

import Control.Monad.Trans.Except
import Lucid hiding (href_)
import Network.HTTP.Types
import Network.Wai
    ( Application, Middleware, Response
    , responseStatus, responseHeaders, responseBuilder
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
import DemoData
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

    runSettings settings . catch404 . serve aulaTopProxy $ aulaTop cfg app


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
  :<|> "user" :> "settings" :> FormHandler PageUserSettings

       -- enter admin api
  :<|> "admin" :> AulaAdmin

       -- delegation network
  :<|> "delegation" :> "edit" :> FormHandlerT PageDelegateVote () --FIXME: Correct page type
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

  :<|> (Frame frameUserHack . PageShow <$> Action.query getUsers)
  :<|> aulaUser
  :<|> form Page.userSettings
  :<|> aulaAdmin

  :<|> error "api not implemented: \"delegation\" :> \"edit\" :> FormHandler ()"
  :<|> makeFrame Page.viewDelegationNetwork

  :<|> pure (Frame frameUserHack PageStaticImprint) -- FIXME: Generate header with menu when the user is logged in.
  :<|> pure (Frame frameUserHack PageStaticTermsOfUse) -- FIXME: Generate header with menu when the user is logged in.

  :<|> form Page.login
  :<|> (logout >> (redirect . absoluteUriPath . relPath $ U.Login))

type IdeaApi
       -- view idea details (applies to both wild ideas and ideas in topics)
    =  Idea ::> "view" :> GetH (Frame ViewIdea)
       -- edit idea (applies to both wild ideas and ideas in topics)
  :<|> Idea ::> "edit" :> FormHandlerT Page.EditIdea Idea
       -- `like' on an idea
  :<|> Idea ::> "like" :> PostH
       -- vote on an idea
  :<|> Idea ::> IdeaVoteValue ::> PostH
       -- comment on an idea
  :<|> Idea ::> "comment" :> FormHandlerT CommentIdea Idea
       -- reply on a comment
  :<|> Idea ::> Comment ::> "reply" :> FormHandlerT CommentIdea Idea
       -- vote on a comment
  :<|> Idea ::> Comment ::> UpDown ::> PostH
       -- vote on a reply of a comment
  :<|> Idea ::> Comment ::> Reply ::> UpDown ::> PostH
       -- delete a comment
  :<|> Idea ::> Comment ::> "delete" :> PostH
       -- delete a comment reply
  :<|> Idea ::> Comment ::> Reply ::> "delete" :> PostH
       -- report a comment
  :<|> Idea ::> Comment ::> "report" :> PostH
       -- report a comment reply
  :<|> Idea ::> Comment ::> Reply ::> "report" :> PostH
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
  :<|> Topic ::> "ideas"              :> IdeasFilterApi :> GetH (Frame ViewTopic)
  :<|> Topic ::> "ideas" :> "all"     :> IdeasFilterApi :> GetH (Frame ViewTopic)
  :<|> Topic ::> "ideas" :> "voting"  :> IdeasFilterApi :> GetH (Frame ViewTopic)
  :<|> Topic ::> "ideas" :> "winning" :> IdeasFilterApi :> GetH (Frame ViewTopic)
  :<|> Topic ::> "delegations"        :> GetH (Frame ViewTopic)

       -- create new topic
  :<|> "topic" :> "create"     :> FormHandler CreateTopic
  :<|> Topic  ::> "idea"       :> "move"   :> FormHandler Page.EditTopic
  :<|> Topic  ::> "delegation" :> "create" :> FormHandler PageDelegateVote

type AulaSpace
    =  IdeaApi
       -- browse wild ideas in an idea space
  :<|> "ideas" :> IdeasFilterApi :> GetH (Frame PageIdeasOverview)
  :<|> TopicApi

ideaApi :: ActionM m => IdeaLocation -> ServerT IdeaApi m
ideaApi loc
    =  makeFrame . Page.viewIdea
  :<|> form . Page.editIdea
  :<|> Action.likeIdea
  :<|> Action.voteIdea
  :<|> (form . Page.commentIdea loc)
  :<|> app2 form (Page.replyCommentIdea loc)
  :<|> Action.voteIdeaComment loc
  :<|> Action.voteIdeaCommentReply loc
  :<|> Action.deleteIdeaComment loc
  :<|> Action.deleteIdeaCommentReply loc
  :<|> Action.reportIdeaComment loc
  :<|> Action.reportIdeaCommentReply loc
  :<|> app2 form Page.judgeIdea
  :<|> form (Page.createIdea loc)

topicApi :: ActionM m => IdeaSpace -> ServerT TopicApi m
topicApi space
    =  makeFrame (Page.viewTopics space)
  :<|> ideaApi        . IdeaLocationTopic space

  :<|> viewTopicTab TabAllIdeas  -- FIXME: if two paths have the same handler, one of them should be a redirect!
  :<|> viewTopicTab TabAllIdeas
  :<|> viewTopicTab TabVotingIdeas
  :<|> viewTopicTab TabWinningIdeas
  :<|> makeFrame . Page.viewTopic TabDelegation

  :<|> form (Page.createTopic space)
  :<|> form . Page.editTopic
  :<|> error "api not implemented: topic/:topic/delegation/create"
  where
    viewTopicTab tab tid qf = makeFrame $ Page.viewTopic (tab qf) tid

aulaSpace :: ActionM m => IdeaSpace -> ServerT AulaSpace m
aulaSpace space
    =  ideaApi (IdeaLocationSpace space)
  :<|> makeFrame . Page.viewIdeas space
  :<|> topicApi                   space

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
  :<|> "access" :> "perm-user-view"    :> GetH (Frame PageAdminSettingsGaPUsersView)
  :<|> "access" :> "perm-user-create"  :> GetH (Frame PageAdminSettingsGaPUsersCreate)
  :<|> "access" :> "perm-class-view"   :> GetH (Frame PageAdminSettingsGaPClassesView)
  :<|> "access" :> "perm-class-create" :> FormHandler PageAdminSettingsGaPClassesCreate
  :<|> User ::> "edit" :> FormHandler PageAdminSettingsGaPUsersEdit
  :<|> SchoolClass ::> "edit" :> GetH (Frame PageAdminSettingsGaPClassesEdit)
       -- event log
  :<|> "event"  :> GetH (Frame PageAdminSettingsEventsProtocol)
  :<|> "passwords" :> Capture "schoolclass" SchoolClass :> Get '[CSV] InitialPasswordsCsvH
  :<|> "events" :> Get '[CSV] EventLog
  :<|> "events" :> Capture "space" IdeaSpace :> Get '[CSV] EventLog


aulaAdmin :: ActionM m => ServerT AulaAdmin m
aulaAdmin =
       form Page.adminDurations
  :<|> form Page.adminQuorum
  :<|> makeFrame Page.adminSettingsGaPUsersView
  :<|> makeFrame Page.adminSettingsGaPUsersCreate
  :<|> makeFrame Page.adminSettingsGaPClassesView
  :<|> form Page.adminSettingsGaPClassesCreate
  :<|> form . Page.adminSettingsGaPUserEdit
  :<|> makeFrame . Page.adminSettingsGaPClassesEdit
  :<|> makeFrame Page.adminEventsProtocol
  :<|> Page.adminInitialPasswordsCsv
  :<|> adminEventLogCsv Nothing
  :<|> adminEventLogCsv . Just

-- | FIXME: this should be in "Frontend.Page.Admin", but that would trigger a cyclical import
-- condition as long as we pull data from Arbitrary rather than from the actual events.
adminEventLogCsv :: ActionM m => Maybe IdeaSpace -> m EventLog
adminEventLogCsv mspc = filterEventLog mspc <$> (viewConfig >>= pure . sampleEventLog)


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
