{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module Frontend
where

import Control.Monad.Trans.Except
import Lucid
import Network.Wai (Application)
import Network.Wai.Handler.Warp (runSettings, setHost, setPort, defaultSettings)
import Network.Wai.Application.Static (StaticSettings, ssRedirectToIndex, ssAddTrailingSlash, ssGetMimeType, defaultFileServerSettings, staticApp)
import Servant
import Servant.HTML.Lucid
import Servant.Missing
import System.FilePath (addTrailingPathSeparator)
import Thentos.Prelude

import Persistent
import Action (Action, mkRunAction, UserState(..))
import Config
import CreateRandom
import Frontend.Page as Page
import Types

import qualified Action


----------------------------------------------------------------------
-- driver

runFrontend :: IO ()
runFrontend = do
    persist <- mkRunPersist
    let action = mkRunAction persist
    unNat persist genInitalTestDb -- FIXME: Remove Bootstrapping DB
    runSettings settings $
      serve (Proxy :: Proxy AulaTop) (aulaTop (action UserLoggedOut))
  where
    settings = setHost (fromString $ Config.config ^. listenerInterface)
             . setPort (Config.config ^. listenerPort)
             $ defaultSettings

----------------------------------------------------------------------
-- driver

type AulaTop =
       (AulaMain :<|> "testing" :> AulaTesting)
  :<|> "samples" :> Raw
  :<|> "static"  :> Raw


aulaTop :: (Action :~> ExceptT ServantErr IO) -> Server AulaTop
aulaTop (Nat runAction) =
       enter runActionForceLogin (aulaMain :<|> aulaTesting)
  :<|> (\req cont -> getSamplesPath >>= \path ->
          waiServeDirectory path req cont)
  :<|> waiServeDirectory (Config.config ^. htmlStatic)
  where
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
       -- user settings
  :<|> "user" :> "settings" :> FormH HTML (Html ()) ()
       -- enter admin api
  :<|> "admin" :> AulaAdmin

       -- delegation network
  :<|> "delegation" :> "edit" :> FormH HTML (Html ()) ()
  :<|> "delegation" :> "view" :> GetH (Frame ST)

       -- static content
  :<|> "imprint" :> GetH (Frame PageStaticImprint)
  :<|> "terms" :> GetH (Frame PageStaticTermsOfUse)

       -- login
  :<|> "login" :> FormH HTML (Html ()) ST


aulaMain :: ServerT AulaMain Action
aulaMain =
       Page.viewRooms
  :<|> aulaSpace

  :<|> (Frame frameUserHack . PageShow <$> Action.persistent getUsers)
  :<|> aulaUser
  :<|> Page.userSettings
  :<|> aulaAdmin

  :<|> error "api not implemented: \"delegation\" :> \"edit\" :> FormH HTML (Html ()) ()"
  :<|> error "api not implemented: \"delegation\" :> \"view\" :> GetH (Frame ST)"

  :<|> pure (Frame frameUserHack PageStaticImprint) -- FIXME: Generate header with menu when the user is logged in.
  :<|> pure (Frame frameUserHack PageStaticTermsOfUse) -- FIXME: Generate header with menu when the user is logged in.

  :<|> Page.login


type AulaSpace =
       -- browse wild ideas in an idea space
       "idea" :> GetH (Frame PageIdeasOverview)
       -- view idea details (applies to both wild ideas and ideas in topics)
  :<|> "idea" :> Capture "idea" (AUID Idea) :> "view" :> GetH (Frame ViewIdea)
       -- edit idea (applies to both wild ideas and ideas in topics)
  :<|> "idea" :> Capture "idea" (AUID Idea) :> "edit" :> FormH HTML (Html ()) Idea
       -- create wild idea
  :<|> "idea" :> "create" :> FormH HTML (Html ()) ST

       -- browse topics in an idea space
  :<|> "topic" :> GetH (Frame PageIdeasInDiscussion)
       -- view topic details (tabs "Alle Ideen", "Beauftragte Stimmen")
  :<|> "topic" :> Capture "topic" (AUID Topic) :> "ideas"              :> GetH (Frame ViewTopic)
  :<|> "topic" :> Capture "topic" (AUID Topic) :> "ideas" :> "all"     :> GetH (Frame ViewTopic)
  :<|> "topic" :> Capture "topic" (AUID Topic) :> "ideas" :> "voting"  :> GetH (Frame ViewTopic)
  :<|> "topic" :> Capture "topic" (AUID Topic) :> "ideas" :> "winning" :> GetH (Frame ViewTopic)
  :<|> "topic" :> Capture "topic" (AUID Topic) :> "delegations"        :> GetH (Frame ViewTopic)
       -- create new topic
  :<|> "topic" :> "create" :> FormH HTML (Html ()) ST
       -- create new idea inside topic
  :<|> "topic" :> Capture "topic" (AUID Topic) :> "idea" :> "create" :> FormH HTML (Html ()) ST
  :<|> "topic" :> Capture "topic" (AUID Topic) :> "idea" :> "move"   :> FormH HTML (Html ()) ST
  :<|> "topic" :> Capture "topic" (AUID Topic) :> "delegation" :> "create" :> FormH HTML (Html ()) ST

aulaSpace :: IdeaSpace -> ServerT AulaSpace Action
aulaSpace space =
       Page.viewIdeas  space
  :<|> Page.viewIdea   space
  :<|> Page.editIdea   space
  :<|> Page.createIdea space

  :<|> Page.viewTopics  space
  :<|> Page.viewTopic   space TabAllIdeas
  :<|> Page.viewTopic   space TabAllIdeas
  :<|> Page.viewTopic   space TabVotingIdeas
  :<|> Page.viewTopic   space TabWinningIdeas
  :<|> Page.viewTopic   space TabDelegation
  :<|> Page.createTopic space []
  :<|> error "api not implemented: \"topic\"  :> \"idea\" :> \"create\" :> FormH HTML (Html ()) ST"
  :<|> Page.moveIdeasToTopic space
  :<|> error "api not implemented: topic/:topic/delegation/create"


type AulaUser =
       "ideas"       :> GetH (PageShow [Idea])
  :<|> "delegations" :> GetH (PageShow [Delegation])

aulaUser :: AUID User -> ServerT AulaUser Action
aulaUser _ =
       error "api not implemented: \"ideas\"       :> GetH (PageShow [Idea])"
  :<|> error "api not implemented: \"delegations\" :> GetH (PageShow [Delegation])"


type AulaAdmin =
       -- durations and quorum
       "params" :> GetH (Frame ST)
       -- groups and permissions
  :<|> "access" :> GetH (Frame ST)
       -- user creation and import
  :<|> "user"   :> GetH (Frame ST)
       -- event log
  :<|> "event"  :> GetH (Frame ST)

aulaAdmin :: ServerT AulaAdmin Action
aulaAdmin =
       error "api not implemented: \"params\" :> GetH (Frame ST)"
  :<|> error "api not implemented: \"access\" :> GetH (Frame ST)"
  :<|> error "api not implemented: \"user\"   :> GetH (Frame ST)"
  :<|> error "api not implemented: \"event\"  :> GetH (Frame ST)"


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
