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
import Network.Wai.Handler.Warp (runSettings, setHost, setPort, defaultSettings)
import Servant
import Servant.HTML.Lucid
import Servant.Missing
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
    runSettings settings . aulaTweaks $ serve (Proxy :: Proxy AulaTop) (aulaTop (action UserLoggedOut))
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
  :<|> (\req cont -> getSamplesPath >>= \path -> serveDirectory path req cont)
  :<|> serveDirectory (Config.config ^. htmlStatic)
  where
    -- FIXME: Login shouldn't happen here
    runActionForceLogin = Nat $ \action -> runAction $ do
        Action.login adminUsernameHack
        action


type AulaMain =
       -- view all spaces
       "space" :> GetH (Frame PageRoomsOverview)
       -- enter one space
  :<|> "space" :> Capture "space" ST :> AulaSpace

       -- view all users
  :<|> "user" :> GetH (Frame (PageShow [User]))
       -- enter user profile
  :<|> "user" :> Capture "user" (AUID User) :> AulaUser
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
       (Frame frameUserHack . PageRoomsOverview <$> Action.persistent getSpaces)
  :<|> error "api not implemented: \"space\" :> Capture \"space\" ST :> AulaSpace"

  :<|> (Frame frameUserHack . PageShow <$> Action.persistent getUsers)
  :<|> aulaUser
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
  :<|> "idea" :> Capture "idea" (AUID Idea) :> "view" :> GetH (Frame PageTopicOverview)
       -- edit idea (applies to both wild ideas and ideas in topics)
  :<|> "idea" :> Capture "idea" (AUID Idea) :> "edit" :> FormH HTML (Html ()) Idea
       -- create wild idea
  :<|> "idea" :> "create" :> FormH HTML (Html ()) ST

       -- browse topics in an idea space
  :<|> "topic" :> GetH (Frame PageIdeasInDiscussion)
       -- view topic details (tabs "Alle Ideen", "Beauftragte Stimmen")
  :<|> "topic" :> Capture "topic" (AUID Topic) :> "ideas"       :> GetH (Frame PageTopicOverview)
  :<|> "topic" :> Capture "topic" (AUID Topic) :> "delegations" :> GetH (Frame PageTopicOverview)
       -- create new topic
  :<|> "topic" :> "create" :> FormH HTML (Html ()) ST
       -- create new idea inside topic
  :<|> "topic" :> Capture "topic" (AUID Topic) :> "idea" :> "create" :> FormH HTML (Html ()) ST

aulaSpace :: ServerT AulaSpace Action
aulaSpace =
       error "api not implemented: \"idea\"   :> GetH (Frame PageIdeasOverview)"
  :<|> error "api not implemented: \"idea\"   :> Capture \"idea\" (AUID Idea) :> GetH (Frame PageTopicOverview)"
  :<|> error "api not implemented: \"idea\"   :> Capture \"idea\" (AUID Idea) :> FormH HTML (Html ()) Idea"
  :<|> error "api not implemented: \"idea\"   :> \"create\" :> FormH HTML (Html ()) ST"

  :<|> error "api not implemented: \"topic\"  :> GetH (Frame PageIdeasInDiscussion)"
  :<|> error "api not implemented: \"topic\"  :> Capture \"topic\" (AUID Topic) :> \"ideas\"       :> GetH (Frame PageTopicOverview)"
  :<|> error "api not implemented: \"topic\"  :> Capture \"topic\" (AUID Topic) :> \"delegations\" :> GetH (Frame PageTopicOverview)"
  :<|> error "api not implemented: \"topic\"  :> \"create\" :> FormH HTML (Html ()) ST"
  :<|> error "api not implemented: \"topic\"  :> \"idea\" :> \"create\" :> FormH HTML (Html ()) ST"


type AulaUser =
       "ideas"       :> GetH (PageShow [Idea])
  :<|> "delegations" :> GetH (PageShow [Delegation])
  :<|> "settings"    :> GetH (Frame ST)

aulaUser :: AUID User -> ServerT AulaUser Action
aulaUser _ =
       error "api not implemented: \"ideas\"       :> GetH (PageShow [Idea])"
  :<|> error "api not implemented: \"delegations\" :> GetH (PageShow [Delegation])"
  :<|> error "api not implemented: \"settings\"    :> GetH (Frame ST)"


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

  :<|> "ideas" :> CreateRandom Idea
  :<|> "space" :> CreateRandom IdeaSpace
  :<|> "topic" :> CreateRandom Topic
  :<|> "user"  :> CreateRandom User

  :<|> "ideas" :> GetH (Frame PageIdeasOverview)
  :<|> "ideas" :> "create" :> FormH HTML (Html ()) ST
  :<|> "ideas" :> "edit" :> Capture "idea" (AUID Idea) :> FormH HTML (Html ()) ST

  :<|> "topics" :> GetH (Frame (PageShow [Topic]))
  :<|> "topics" :> Capture "topic" (AUID Topic) :> GetH (Frame PageTopicOverview)
  :<|> "topics" :> "create" :> FormH HTML (Html ()) ST

aulaTesting :: ServerT AulaTesting Action
aulaTesting =
       return (PublicFrame "yihaah!")

  :<|> createRandom dbIdeaMap
  :<|> createRandomNoMeta dbSpaceSet
  :<|> createRandom dbTopicMap
  :<|> createRandom dbUserMap

  :<|> (Frame frameUserHack . PageIdeasOverview SchoolSpace <$> Action.persistent getIdeas)
  :<|> Page.createIdea
  :<|> Page.editIdea

  :<|> (Frame frameUserHack . PageShow <$> Action.persistent getTopics)
  :<|> Page.pageTopicOverview
  :<|> Page.createTopic
