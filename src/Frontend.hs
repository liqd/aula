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
import System.FilePath (addTrailingPathSeparator)
import Thentos.Prelude

import qualified Data.ByteString.Builder as Builder

import Action (Action, mkRunAction, UserState(..))
import Config
import CreateRandom
import Frontend.Page as Page
import Persistent
import Types

import qualified Action


----------------------------------------------------------------------
-- driver

runFrontend :: Config.Config -> IO ()
runFrontend cfg = do
    persist <- mkRunPersist
    let action = mkRunAction persist
        proxy  = Proxy :: Proxy AulaTop
    unNat persist genInitialTestDb -- FIXME: Remove Bootstrapping DB
    runSettings settings . catch404 . serve proxy . aulaTop $ action UserLoggedOut
  where
    settings = setHost (fromString $ cfg ^. listenerInterface)
             . setPort (cfg ^. listenerPort)
             $ defaultSettings

----------------------------------------------------------------------
-- driver

type AulaTop =
       (AulaMain :<|> "testing" :> AulaTesting)
  :<|> "samples" :> Raw
  :<|> "static"  :> Raw
  :<|> GetH (Frame ())


aulaTop :: (Action :~> ExceptT ServantErr IO) -> Server AulaTop
aulaTop (Nat runAction) =
       enter runActionForceLogin (catchAulaExcept proxy (aulaMain :<|> aulaTesting))
  :<|> (\req cont -> getSamplesPath >>= \path ->
          waiServeDirectory path req cont)
  :<|> waiServeDirectory (Config.config ^. htmlStatic)
  :<|> redirect "/space"
  where
    proxy :: Proxy (AulaMain :<|> "testing" :> AulaTesting)
    proxy = Proxy

    -- FIXME: Login shouldn't happen here
    runActionForceLogin = Nat $ \action -> runAction $ do
        Action.login adminUsernameHack
        action

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

type AulaMain =
       -- view all spaces
       "space" :> GetH (Frame PageRoomsOverview)
       -- enter one space
  :<|> "space" :> Capture "space" IdeaSpace :> AulaSpace

       -- view all users
  :<|> "user" :> GetH (Frame (PageShow [User]))
       -- enter user profile

  :<|> "user" :> Capture "user" (AUID User) :> AulaUser
  :<|> "user" :> "settings" :> FormHandler PageUserSettings ST
       -- enter admin api
  :<|> "admin" :> AulaAdmin

       -- delegation network
  :<|> "delegation" :> "edit" :> FormHandler PageDelegateVote () --FIXME: Correct page type
  :<|> "delegation" :> "view" :> GetH (Frame PageDelegationNetwork)

       -- static content
  :<|> "imprint" :> GetH (Frame PageStaticImprint)
  :<|> "terms" :> GetH (Frame PageStaticTermsOfUse)

       -- login
  :<|> "login" :> FormHandler PageHomeWithLoginPrompt ST
  :<|> "logout" :> GetH (Frame PageLogout)


aulaMain :: ServerT AulaMain Action
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
  :<|> Page.logout


type AulaSpace =
       -- browse wild ideas in an idea space
       "idea" :> GetH (Frame PageIdeasOverview)
       -- view idea details (applies to both wild ideas and ideas in topics)
  :<|> "idea" :> Capture "idea" (AUID Idea) :> "view" :> GetH (Frame ViewIdea)
       -- edit idea (applies to both wild ideas and ideas in topics)
  :<|> "idea" :> Capture "idea" (AUID Idea) :> "edit" :> FormHandler EditIdea Idea
       -- create wild idea
  :<|> "idea" :> "create" :> FormHandler CreateIdea ST

       -- browse topics in an idea space
  :<|> "topic" :> GetH (Frame PageIdeasInDiscussion)
       -- view topic details (tabs "Alle Ideen", "Beauftragte Stimmen")
  :<|> "topic" :> Capture "topic" (AUID Topic) :> "ideas"              :> GetH (Frame ViewTopic)
  :<|> "topic" :> Capture "topic" (AUID Topic) :> "ideas" :> "all"     :> GetH (Frame ViewTopic)
  :<|> "topic" :> Capture "topic" (AUID Topic) :> "ideas" :> "voting"  :> GetH (Frame ViewTopic)
  :<|> "topic" :> Capture "topic" (AUID Topic) :> "ideas" :> "winning" :> GetH (Frame ViewTopic)
  :<|> "topic" :> Capture "topic" (AUID Topic) :> "delegations"        :> GetH (Frame ViewTopic)
       -- create new topic
  :<|> "topic" :> "create" :> FormHandler CreateTopic ST
       -- create new idea inside topic
  :<|> "topic" :> Capture "topic" (AUID Topic) :> "idea" :> "create" :> FormHandler CreateIdea ST
  :<|> "topic" :> Capture "topic" (AUID Topic) :> "idea" :> "move"   :> FormHandler MoveIdeasToTopic ST
  :<|> "topic" :> Capture "topic" (AUID Topic)
               :> "delegation" :> "create" :> FormHandler PageDelegateVote ST --FIXME: Change Type

aulaSpace :: IdeaSpace -> ServerT AulaSpace Action
aulaSpace space =
       Page.viewIdeas  space
  :<|> Page.viewIdea   space
  :<|> Page.editIdea   space
  :<|> Page.createIdea space Nothing

  :<|> Page.viewTopics  space
  :<|> Page.viewTopic   space TabAllIdeas
  :<|> Page.viewTopic   space TabAllIdeas
  :<|> Page.viewTopic   space TabVotingIdeas
  :<|> Page.viewTopic   space TabWinningIdeas
  :<|> Page.viewTopic   space TabDelegation
  :<|> Page.createTopic space []
  :<|> Page.createIdea  space . Just
  :<|> Page.moveIdeasToTopic space
  :<|> error "api not implemented: topic/:topic/delegation/create"


type AulaUser =
       "ideas"       :> GetH (Frame PageUserProfileCreatedIdeas)
  :<|> "delegations" :> GetH (Frame PageUserProfileDelegatedVotes)

aulaUser :: AUID User -> ServerT AulaUser Action
aulaUser user =
       Page.createdIdeas   user
  :<|> Page.delegatedVotes user


type AulaAdmin =
       -- durations
       "duration" :> FormHandler PageAdminSettingsDurations ST
       -- quorum
  :<|> "quorum" :> FormHandler PageAdminSettingsQuorum ST
       -- groups and permissions
  :<|> "access" :> Capture "context" PermissionContext :> GetH (Frame PageAdminSettingsGroupsAndPermissions)
       -- user creation and import
  :<|> "user"   :> FormHandler PageAdminSettingsUserCreateAndImport ST
       -- event log
  :<|> "event"  :> GetH (Frame PageAdminSettingsEventsProtocol)


aulaAdmin :: ServerT AulaAdmin Action
aulaAdmin =
       Page.adminDurations
  :<|> Page.adminQourum
  :<|> Page.adminSettingsGroupsAndPermissions
  :<|> Page.adminSettingsUserCreateAndImport
  :<|> Page.adminEventsProtocol

type AulaTesting =
       GetH (Frame ST)

  :<|> "idea"  :> CreateRandom Idea
  :<|> "space" :> CreateRandom IdeaSpace
  :<|> "topic" :> CreateRandom Topic
  :<|> "user"  :> CreateRandom User

  :<|> "ideas"  :> GetH (Frame (PageShow [Idea]))
  :<|> "spaces" :> GetH (Frame (PageShow [IdeaSpace]))
  :<|> "topics" :> GetH (Frame (PageShow [Topic]))
  :<|> "users"  :> GetH (Frame (PageShow [User]))

  :<|> "file-upload" :> FormHandler BatchCreateUsers ST

aulaTesting :: ServerT AulaTesting Action
aulaTesting =
       return (PublicFrame "yihaah!")

  :<|> createRandom dbIdeaMap
  :<|> createRandomNoMeta dbSpaceSet
  :<|> createRandom dbTopicMap
  :<|> createRandom dbUserMap

  :<|> (PublicFrame . PageShow <$> Action.persistent getIdeas)
  :<|> (PublicFrame . PageShow <$> Action.persistent getSpaces)
  :<|> (PublicFrame . PageShow <$> Action.persistent getTopics)
  :<|> (PublicFrame . PageShow <$> Action.persistent getUsers)

  :<|> batchCreateUsers


----------------------------------------------------------------------
-- error handling in servant / wai

-- | (The proxy in the type of this function helps dealing with injectivity issues with the `Server`
-- type family.)
catchAulaExcept :: (m a ~ (ServerT api Action)) => Proxy api -> m a -> m a
catchAulaExcept Proxy = id
-- FIXME: not implemented.  pseudo-code:
--
--   ... = (`catchError` actionExceptHandler)
--  where
--    actionExceptHandler :: ActionExcept -> s
--    actionExceptHandler = undefined
--
-- -- (async exceptions (`error` and all) should be caught inside module "Action" and exposed as
-- -- `err500` here.)

data Page404 = Page404

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
