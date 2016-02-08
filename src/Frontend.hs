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

import Control.Lens ((^.))
import Control.Monad.IO.Class (liftIO)
import Data.String (fromString)
import Network.Wai.Handler.Warp (runSettings, setHost, setPort, defaultSettings)
import Servant
import Servant.HTML.Blaze
import Test.QuickCheck

import Api.NoPersistent
import Config
import Frontend.Html
import Arbitrary ()


runFrontend :: IO ()
runFrontend = runSettings settings $ serve (Proxy :: Proxy FrontendH) frontendH
  where
    settings = setHost (fromString $ Config.config ^. listenerInterface)
             . setPort (Config.config ^. listenerPort)
             $ defaultSettings

type GetH = Get '[HTML]

type FrontendH =
       GetH (Frame String)
  :<|> "ideas" :> "create_random" :> GetH (Frame String)
  :<|> "ideas" :> GetH (Frame PageIdeasOverview)

--  :<|> "ideas" "> "create" :> FromH

  :<|> Raw

frontendH :: Server FrontendH
frontendH =
       return (Frame "yihaah!")
  :<|> (liftIO (generate arbitrary) >>= addIdeaH >> return (Frame "new idea created."))
  :<|> (Frame . PageIdeasOverview <$> getIdeasH)

  :<|> serveDirectory (Config.config ^. htmlStatic)
