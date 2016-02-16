{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module Frontend
where

import Lucid
import Network.Wai.Handler.Warp (runSettings, setHost, setPort, defaultSettings)
import Servant
import Servant.HTML.Lucid
import Servant.Missing
import Thentos.Prelude

import Persistent
import Arbitrary ()
import Config
import CreateRandom
import Frontend.Pages as Pages
import Types

runFrontend :: IO ()
runFrontend = runSettings settings . aulaTweaks $ serve (Proxy :: Proxy FrontendH) frontendH
  where
    settings = setHost (fromString $ Config.config ^. listenerInterface)
             . setPort (Config.config ^. listenerPort)
             $ defaultSettings

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
  :<|> Raw

render :: MonadIO m => Persist body -> m (Frame body)
render m = liftIO . runPersist $ Frame <$> m

frontendH :: Server FrontendH
frontendH =
       return (Frame "yihaah!")
  :<|> Pages.login
  :<|> createRandom "idea" dbIdeaMap
  :<|> render (PageIdeasOverview <$> getIdeas)
  :<|> Pages.createIdea
  :<|> createRandom "user" dbUserMap
  :<|> render (PageShow <$> getUsers)
  :<|> createRandom "topic" dbTopicMap
  :<|> render (PageShow <$> getTopics)
  :<|> Pages.pageTopicOverview
  :<|> serveDirectory (Config.config ^. htmlStatic)
