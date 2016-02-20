{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

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
import Frontend.Page as Page
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
  :<|> "topics" :> "create" :> FormH HTML (Html ()) ST
  :<|> "imprint" :> GetH (Frame PageStaticImprint)
  :<|> "terms" :> GetH (Frame PageStaticTermsOfUse)
  :<|> Raw

render :: (Page body, MonadIO m) => Persist body -> m (Frame body)
render m = liftIO . runPersist $ makeFrame <$> m

frontendH :: Server FrontendH
frontendH =
       return (PublicFrame "yihaah!")
  :<|> Page.login
  :<|> createRandom "idea" dbIdeaMap
  :<|> render (PageIdeasOverview <$> getIdeas)
  :<|> Page.createIdea
  :<|> createRandom "user" dbUserMap
  :<|> render (PageShow <$> getUsers)
  :<|> createRandom "topic" dbTopicMap
  :<|> render (PageShow <$> getTopics)
  :<|> Page.pageTopicOverview
  :<|> Page.createTopic
  :<|> render (pure PageStaticImprint) -- FIXME: Generate header with menu when the user is logged in.
  :<|> render (pure PageStaticTermsOfUse) -- FIXME: Generate header with menu when the user is logged in.
  :<|> serveDirectory (Config.config ^. htmlStatic)
