{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
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
import Frontend.Core
import Frontend.Html
import Types

import qualified Frontend.Page.CreateIdea as Page

runFrontend :: IO ()
runFrontend = runSettings settings . aulaTweaks $ serve (Proxy :: Proxy FrontendH) frontendH
  where
    settings = setHost (fromString $ Config.config ^. listenerInterface)
             . setPort (Config.config ^. listenerPort)
             $ defaultSettings

type GetH = Get '[HTML]

type FrontendH =
       GetH (Frame ST)
  :<|> "ideas" :> GetH (Frame PageIdeasOverview)
  :<|> "ideas" :> "create" :> FormH HTML (Html ()) ST
  :<|> "users" :> GetH (Frame (PageShow [User]))
  :<|> "login" :> Capture "login" ST :> GetH (Frame ST)
  :<|> Raw

render :: MonadIO m => Persist body -> m (Frame body)
render m = liftIO . runPersist $ Frame <$> m

frontendH :: Server FrontendH
frontendH =
       return (Frame "yihaah!")
  :<|> render (PageIdeasOverview <$> getIdeas)
  :<|> Page.createIdea
  :<|> render (PageShow <$> getUsers)
  :<|> (\login -> liftIO . runPersist $ Frame ("You are now logged in as " <> login) <$ loginUser login)
  :<|> serveDirectory (Config.config ^. htmlStatic)
