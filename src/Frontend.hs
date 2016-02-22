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
import Arbitrary (arbitrary, generate)
import Config
import CreateRandom
import Frontend.Page as Page
import Types

import qualified Action

-- FIXME: generate a proper user here, with real time stamp and AUID and everything.  no need to use
-- arbitrary in 'bootstrapDB' below!
adminUsernameHack :: ST
adminUsernameHack = "admin"

runFrontend :: IO ()
runFrontend = do
    persist <- mkRunPersist
    let action = mkRunAction persist
    bootsrapDB persist -- FIXME: Remove Bootstrapping DB
    runSettings settings . aulaTweaks $ serve (Proxy :: Proxy Aula) (aula (action UserLoggedOut))
  where
    settings = setHost (fromString $ Config.config ^. listenerInterface)
             . setPort (Config.config ^. listenerPort)
             $ defaultSettings

    -- FIXME: Remove Bootstrapping DB
    bootsrapDB :: Persist :~> IO -> IO ()
    bootsrapDB persist =
        generate arbitrary >>= void . bootstrapUser persist . (userLogin .~ adminUsernameHack)

type GetH = Get '[HTML]

-- FIXME: this should be in module "CreateRandom".
type CreateRandom a = "create_random" :> GetH (Frame (ST `Beside` PageShow a))

type FrontendH =
       GetH (Frame ST)
  :<|> "ideaspaces" :> GetH (Frame PageRoomsOverview)
  :<|> "ideaspaces" :> CreateRandom IdeaSpace
  :<|> "login" :> FormH HTML (Html ()) ST
  :<|> "ideas" :> CreateRandom Idea
  :<|> "ideas" :> GetH (Frame PageIdeasOverview)
  :<|> "ideas" :> "create" :> FormH HTML (Html ()) ST
  :<|> "users" :> CreateRandom User
  :<|> "users" :> GetH (Frame (PageShow [User]))
  :<|> "topics" :> CreateRandom Topic
  :<|> "topics" :> GetH (Frame (PageShow [Topic]))
  :<|> "topics" :> Capture "topic" (AUID Topic) :> GetH (Frame PageTopicOverview)
  :<|> "topics" :> "create" :> FormH HTML (Html ()) ST
  :<|> "imprint" :> GetH (Frame PageStaticImprint)
  :<|> "terms" :> GetH (Frame PageStaticTermsOfUse)

type Aula =
       FrontendH
  :<|> "samples" :> Raw
  :<|> "static"  :> Raw

aula :: (Action :~> ExceptT ServantErr IO) -> Server Aula
aula (Nat runAction) =
       enter runActionForceLogin frontendH
  :<|> (\req cont -> getSamplesPath >>= \path -> serveDirectory path req cont)
  :<|> serveDirectory (Config.config ^. htmlStatic)
  where
    -- FIXME: Login shouldn't happen here
    runActionForceLogin = Nat $ \action -> runAction $ do
        Action.login adminUsernameHack
        action

frontendH :: ServerT FrontendH Action
frontendH =
       return (PublicFrame "yihaah!")
  :<|> (Frame frameUserHack . PageRoomsOverview <$> Action.persistent getSpaces)
  :<|> createRandomNoMeta dbSpaceSet
  :<|> Page.login
  :<|> createRandom dbIdeaMap
  :<|> (Frame frameUserHack . PageIdeasOverview <$> Action.persistent getIdeas)
  :<|> Page.createIdea
  :<|> createRandom dbUserMap
  :<|> (Frame frameUserHack . PageShow <$> Action.persistent getUsers)
  :<|> createRandom dbTopicMap
  :<|> (Frame frameUserHack . PageShow <$> Action.persistent getTopics)
  :<|> Page.pageTopicOverview
  :<|> Page.createTopic
  :<|> pure (Frame frameUserHack PageStaticImprint) -- FIXME: Generate header with menu when the user is logged in.
  :<|> pure (Frame frameUserHack PageStaticTermsOfUse) -- FIXME: Generate header with menu when the user is logged in.
