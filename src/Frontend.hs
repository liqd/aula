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
import Network.Wai.Handler.Warp (runSettings, setHost, setPort, defaultSettings)
import Network.Wai.Application.Static
    ( StaticSettings
    , ssRedirectToIndex, ssAddTrailingSlash, ssGetMimeType, defaultFileServerSettings, staticApp
    )
import Servant
import Servant.Missing (throwError500, throwServantErr)
import System.FilePath (addTrailingPathSeparator)

import qualified Data.ByteString.Builder as Builder

import Thentos.Prelude
import Thentos.Types (ThentosSessionToken)
import Thentos.Frontend.State (serveFAction)

import Action (UserState, ActionEnv(..), logout)
import Action.Implementation (Action, mkRunAction)
import Config
import CreateRandom
import Data.UriPath
import Frontend.Core
import Frontend.Page as Page
import Persistent
import Types

import qualified Action
import qualified Backend
import qualified Frontend.Path as U
import qualified Persistent.Implementation


-- * driver

extendClearanceOnSessionToken :: Applicative m => ThentosSessionToken -> m ()
extendClearanceOnSessionToken _ = pure () -- FIXME

runFrontend :: Config -> IO ()
runFrontend cfg = do
    withPersist Persistent.Implementation.mkRunPersist
                (runFrontendGeneric cfg)

-- | Run the frontend with the given persitence implementation
-- (e.g., in-memory or on-disk) and config.
runFrontendGeneric :: Config
                   -> (Persistent.Implementation.Persist :~> ExceptT PersistExcept IO)
                   -> IO ()
runFrontendGeneric cfg rp = do
    let runAction :: Action Persistent.Implementation.Persist :~> ExceptT ServantErr IO
        runAction = mkRunAction (ActionEnv rp cfg)

        aulaTopProxy = Proxy :: Proxy AulaTop
        stateProxy   = Proxy :: Proxy UserState

    app <- serveFAction (Proxy :: Proxy AulaActions) stateProxy extendClearanceOnSessionToken
        runAction aulaActions

    -- Note that no user is being logged in anywhere here.
    runSettings settings . catch404 . serve aulaTopProxy $ aulaTop cfg app

  where
    settings = setHost (fromString $ cfg ^. listenerInterface)
             . setPort (cfg ^. listenerPort)
             $ defaultSettings


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

aulaActions :: (GenArbitrary r, PersistM r) => ServerT AulaActions (Action r)
aulaActions =
       aulaMain
  :<|> Backend.api
  :<|> aulaTesting


type AulaMain =
       -- view all spaces
       "space" :> GetH (Frame PageRoomsOverview)
       -- enter one space
  :<|> "space" :> Capture "space" IdeaSpace :> AulaSpace

       -- view all users
  :<|> "user" :> GetH (Frame (PageShow [User]))
       -- enter user profile

  :<|> "user" :> Capture "user" (AUID User) :> AulaUser
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


aulaMain :: PersistM r => ServerT AulaMain (Action r)
aulaMain =
       Page.viewRooms
  :<|> aulaSpace

  :<|> (Frame frameUserHack . PageShow <$> Action.persistent getUsers)
  :<|> aulaUser
  :<|> Page.userSettings
  :<|> aulaAdmin

  :<|> error "api not implemented: \"delegation\" :> \"edit\" :> FormHandler ()"
  :<|> Page.viewDelegationNetwork

  :<|> pure (Frame frameUserHack PageStaticImprint) -- FIXME: Generate header with menu when the user is logged in.
  :<|> pure (Frame frameUserHack PageStaticTermsOfUse) -- FIXME: Generate header with menu when the user is logged in.

  :<|> Page.login
  :<|> (logout >> (redirect . absoluteUriPath . relPath $ U.Login))


type AulaSpace =
       -- browse wild ideas in an idea space
       "ideas" :> GetH (Frame PageIdeasOverview)
       -- view idea details (applies to both wild ideas and ideas in topics)
  :<|> "idea" :> Capture "idea" (AUID Idea) :> "view" :> GetH (Frame ViewIdea)
       -- edit idea (applies to both wild ideas and ideas in topics)
  :<|> "idea" :> Capture "idea" (AUID Idea) :> "edit" :> FormHandlerT EditIdea Idea
       -- `like' on an idea
  :<|> "idea" :> Capture "idea" (AUID Idea) :> "like" :> PostH
       -- vote on an idea
  :<|> "idea" :> Capture "idea" (AUID Idea) :> "vote" :> Capture "vote" IdeaVoteValue :> PostH
       -- comment on an idea
  :<|> "idea" :> Capture "idea" (AUID Idea) :> "comment" :> FormHandlerT CommentIdea Idea
       -- reply on a comment
  :<|> "idea" :> Capture "idea" (AUID Idea) :> "comment" :> Capture "comment" (AUID Comment)
                                            :> "reply"   :> FormHandlerT CommentIdea Idea
       -- vote on a comment
  :<|> "idea" :> Capture "idea" (AUID Idea) :> "comment" :> Capture "comment" (AUID Comment)
                                            :> "vote"    :> Capture "vote" UpDown :> PostH
       -- vote on a reply of a comment
  :<|> "idea" :> Capture "idea" (AUID Idea) :> "comment" :> Capture "comment" (AUID Comment)
              :> "reply" :> Capture "reply" (AUID Comment)
              :> "vote" :> Capture "vote" UpDown :> PostH
       -- create wild idea
  :<|> "idea" :> "create" :> FormHandler CreateIdea

       -- browse topics in an idea space
  :<|> "topic" :> GetH (Frame PageIdeasInDiscussion)
       -- view topic details (tabs "Alle Ideen", "Beauftragte Stimmen")
  :<|> "topic" :> Capture "topic" (AUID Topic) :> "ideas"              :> GetH (Frame ViewTopic)
  :<|> "topic" :> Capture "topic" (AUID Topic) :> "ideas" :> "all"     :> GetH (Frame ViewTopic)
  :<|> "topic" :> Capture "topic" (AUID Topic) :> "idea" :> Capture "idea" (AUID Idea)
               :> "view" :> GetH (Frame ViewIdea)
  :<|> "topic" :> Capture "topic" (AUID Topic) :> "idea" :> Capture "idea" (AUID Idea)
               :> "edit" :> FormHandler EditIdea
  :<|> "topic" :> Capture "topic" (AUID Topic) :> "idea" :> Capture "idea" (AUID Idea)
               :> "like" :> PostH
  :<|> "topic" :> Capture "topic" (AUID Topic) :> "idea" :> Capture "idea" (AUID Idea)
               :> "vote" :> Capture "vote" IdeaVoteValue :> PostH
  :<|> "topic" :> Capture "topic" (AUID Topic) :> "idea" :> Capture "idea" (AUID Idea)
               :> "comment" :> FormHandler CommentIdea
  :<|> "topic" :> Capture "topic" (AUID Topic) :> "idea" :> Capture "idea" (AUID Idea)
               :> "comment" :> Capture "comment" (AUID Comment)
               :> "reply" :> FormHandlerT CommentIdea Idea
  :<|> "topic" :> Capture "topic" (AUID Topic) :> "idea" :> Capture "idea" (AUID Idea)
               :> "comment" :> Capture "comment" (AUID Comment)
               :> "vote" :> Capture "vote" UpDown :> PostH
  :<|> "topic" :> Capture "topic" (AUID Topic) :> "idea" :> Capture "idea" (AUID Idea)
               :> "comment" :> Capture "comment" (AUID Comment)
               :> "reply" :> Capture "reply" (AUID Comment)
               :> "vote" :> Capture "vote" UpDown :> PostH
  :<|> "topic" :> Capture "topic" (AUID Topic) :> "idea" :> "create"   :> FormHandler CreateIdea
  :<|> "topic" :> Capture "topic" (AUID Topic) :> "ideas" :> "voting"  :> GetH (Frame ViewTopic)
  :<|> "topic" :> Capture "topic" (AUID Topic) :> "ideas" :> "winning" :> GetH (Frame ViewTopic)
  :<|> "topic" :> Capture "topic" (AUID Topic) :> "delegations"        :> GetH (Frame ViewTopic)

       -- create new topic
  :<|> "topic" :> "create" :> FormHandler CreateTopic
  :<|> "topic" :> Capture "topic" (AUID Topic) :> "idea" :> "move"   :> FormHandler EditTopic
  :<|> "topic" :> Capture "topic" (AUID Topic)
               :> "delegation" :> "create" :> FormHandler PageDelegateVote

aulaSpace :: PersistM r => IdeaSpace -> ServerT AulaSpace (Action r)
aulaSpace space =
       Page.viewIdeas  space
  :<|> Page.viewIdea
  :<|> Page.editIdea
  :<|> Action.likeIdea
  :<|> Action.voteIdea
  :<|> Page.commentIdea
  :<|> Page.replyCommentIdea
  :<|> Action.voteIdeaComment
  :<|> Action.voteIdeaCommentReply
  :<|> Page.createIdea  locSpace

  :<|> Page.viewTopics  space
  :<|> Page.viewTopic   TabAllIdeas  -- FIXME: if two paths have the same handler, one of them should be a redirect!
  :<|> Page.viewTopic   TabAllIdeas
  :<|> const Page.viewIdea
  :<|> const Page.editIdea
  :<|> const Action.likeIdea
  :<|> const Action.voteIdea
  :<|> const Page.commentIdea
  :<|> const Page.replyCommentIdea
  :<|> const Action.voteIdeaComment
  :<|> const Action.voteIdeaCommentReply
  :<|> Page.createIdea  . locTopic
  :<|> Page.viewTopic   TabVotingIdeas
  :<|> Page.viewTopic   TabWinningIdeas
  :<|> Page.viewTopic   TabDelegation

  :<|> Page.createTopic space
  :<|> Page.editTopic
  :<|> error "api not implemented: topic/:topic/delegation/create"

  where locSpace = IdeaLocationSpace space
        locTopic = IdeaLocationTopic space

type AulaUser =
       "ideas"       :> GetH (Frame PageUserProfileCreatedIdeas)
  :<|> "delegations" :> GetH (Frame PageUserProfileDelegatedVotes)

aulaUser :: PersistM r => AUID User -> ServerT AulaUser (Action r)
aulaUser user =
       Page.createdIdeas   user
  :<|> Page.delegatedVotes user


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
  :<|> "user" :> Capture "user" (AUID User) :> "edit" :> FormHandler PageAdminSettingsGaPUsersEdit
  :<|> "class" :> Capture "class" SchoolClass :> "edit" :> GetH (Frame PageAdminSettingsGaPClassesEdit)
       -- event log
  :<|> "event"  :> GetH (Frame PageAdminSettingsEventsProtocol)


aulaAdmin :: PersistM r => ServerT AulaAdmin (Action r)
aulaAdmin =
       Page.adminDurations
  :<|> Page.adminQuorum
  :<|> Page.adminSettingsGaPUsersView
  :<|> Page.adminSettingsGaPUsersCreate
  :<|> Page.adminSettingsGaPClassesView
  :<|> Page.adminSettingsGaPClassesCreate
  :<|> Page.adminSettingsGaPUserEdit
  :<|> Page.adminSettingsGaPClassesEdit
  :<|> Page.adminEventsProtocol

type AulaTesting =
       "idea"  :> CreateRandom Idea
  :<|> "space" :> CreateRandom IdeaSpace
  :<|> "topic" :> CreateRandom Topic

  :<|> "ideas"  :> GetH (Frame (PageShow [Idea]))
  :<|> "spaces" :> GetH (Frame (PageShow [IdeaSpace]))
  :<|> "topics" :> GetH (Frame (PageShow [Topic]))
  :<|> "users"  :> GetH (Frame (PageShow [User]))

  :<|> "random-password" :> GetH (PageShow UserPass)
  :<|> "undefined" :> GetH ()
  :<|> "error500" :> GetH ()
  :<|> "error303" :> GetH ()

aulaTesting :: (GenArbitrary r, PersistM r) => ServerT AulaTesting (Action r)
aulaTesting =
       createRandom dbIdeaMap
  :<|> createRandomNoMeta dbSpaceSet
  :<|> createRandom dbTopicMap

  :<|> (PublicFrame . PageShow <$> Action.persistent getIdeas)
  :<|> (PublicFrame . PageShow <$> Action.persistent getSpaces)
  :<|> (PublicFrame . PageShow <$> Action.persistent getTopics)
  :<|> (PublicFrame . PageShow <$> Action.persistent getUsers)

  :<|> (PageShow <$> Action.persistent mkRandomPassword)
  :<|> undefined
  :<|> throwError500 "testing error500"
  :<|> throwServantErr (err303 { errHeaders = ("Location", "/target") : errHeaders err303 })

data Page404 = Page404

instance Page Page404 where
    isPrivatePage _ = False

instance ToHtml Page404 where
    toHtmlRaw = toHtml
    toHtml Page404 = div_ $ p_ "404"

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
