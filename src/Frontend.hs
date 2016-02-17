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

adminUsername :: ST
adminUsername = "admin"

runFrontend :: IO ()
runFrontend = do
    persist <- mkRunPersist
    let action = mkRunAction persist
    bootsrapDB persist -- FIXME: Remove Bootstrapping DB
    runSettings settings . aulaTweaks $ serve (Proxy :: Proxy Aula) (aula (action UnknownUser))
  where
    settings = setHost (fromString $ Config.config ^. listenerInterface)
             . setPort (Config.config ^. listenerPort)
             $ defaultSettings

    -- FIXME: Remove Bootstrapping DB
    bootsrapDB :: Persist :~> IO -> IO ()
    bootsrapDB persist =
        generate arbitrary >>= void . bootstrapUser persist . (userLogin .~ adminUsername)

type GetH = Get '[HTML]

type CreateRandom a = "create_random" :> GetH (Frame (ST `Beside` PageShow a))

type FrontendH =
       GetH (Frame ST)
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
  :<|> Raw

aula :: (Action :~> ExceptT ServantErr IO) -> Server Aula
aula (Nat runAction) =
       enter runActionForceLogin frontendH
  :<|> serveDirectory (Config.config ^. htmlStatic)
  where
    -- FIXME: Login shouldn't happen here
    runActionForceLogin = Nat $ \action -> runAction $ do
        Action.login adminUsername
        action

frontendH :: ServerT FrontendH Action
frontendH =
       return (PublicFrame "yihaah!")
  :<|> Page.login
  :<|> createRandom "idea" dbIdeaMap
  :<|> (Frame frameUserHack . PageIdeasOverview <$> Action.persistent getIdeas)
  :<|> Page.createIdea
  :<|> createRandom "user" dbUserMap
  :<|> (Frame frameUserHack . PageShow <$> Action.persistent getUsers)
  :<|> createRandom "topic" dbTopicMap
  :<|> (Frame frameUserHack . PageShow <$> Action.persistent getTopics)
  :<|> Page.pageTopicOverview
  :<|> Page.createTopic
  :<|> (pure $ Frame frameUserHack PageStaticImprint) -- FIXME: Generate header with menu when the user is logged in.
  :<|> (pure $ Frame frameUserHack PageStaticTermsOfUse) -- FIXME: Generate header with menu when the user is logged in.
