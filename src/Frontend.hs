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

import Control.Monad.Trans.Except (ExceptT)
import Lucid
import Network.Wai.Handler.Warp (runSettings, setHost, setPort, defaultSettings)
import Servant
import Servant.HTML.Lucid
import System.IO.Unsafe (unsafePerformIO)
import Test.QuickCheck (Arbitrary, generate, arbitrary)
import Text.Digestive.Form ((.:))
import Text.Digestive.View (View)

import qualified Text.Digestive.Form as DF
import qualified Text.Digestive.Lucid.Html5 as DF

import Servant.Missing
import Thentos.Prelude

import Api.Persistent
import Arbitrary ()
import Config
import Frontend.Html
import Types


runFrontend :: IO ()
runFrontend = runSettings settings $ serve (Proxy :: Proxy FrontendH) frontendH
  where
    settings = setHost (fromString $ Config.config ^. listenerInterface)
             . setPort (Config.config ^. listenerPort)
             $ defaultSettings

-- | FIXME: this should be in moved to a state object that is passed down from 'runFrontend'.
runPersist :: Persist a -> IO a
runPersist = unNat $ unsafePerformIO mkRunPersist

type GetH = Get '[HTML]

type FrontendH =
       GetH (Frame ST)
  :<|> "ideas" :> "create_random" :> GetH (Frame ST)
  :<|> "ideas" :> GetH (Frame PageIdeasOverview)
  :<|> "ideas" :> "create" :> FormH HTML (Html ()) ST
  :<|> "users" :> "create_random" :> GetH (Frame ST)
  :<|> "users" :> GetH (Frame (PageShow [User]))
  :<|> "login" :> Capture "login" ST :> GetH (Frame ST)
  :<|> Raw

createRandom :: (MonadIO m, Arbitrary a) => ST -> Lens' AulaData [a] -> m (Frame ST)
createRandom s l =
  liftIO $ generate arbitrary >>= runPersist . addDb l >> return (Frame ("new " <> s <> " created."))

render :: MonadIO m => Persist body -> m (Frame body)
render m = liftIO . runPersist $ Frame <$> m

frontendH :: Server FrontendH
frontendH =
       return (Frame "yihaah!")
  :<|> createRandom "idea" dbIdeas
  :<|> render (PageIdeasOverview <$> getIdeas)
  :<|> myFirstForm
  :<|> createRandom "user" dbUsers
  :<|> render (PageShow <$> getUsers)
  :<|> (\login -> liftIO . runPersist $ Frame ("You are now logged in as " <> login) <$ loginUser login)
  :<|> serveDirectory (Config.config ^. htmlStatic)


-- FIXME: would it be possible to have to html type params for 'FormH'?  one for the result of r,
-- and one for the result of p2?  then the result of p2 could have any 'ToHtml' instance.
myFirstForm :: Server (FormH HTML (Html ()) ST)
myFirstForm = formH "/ideas/create" p1 p2 r
  where
    p1 :: DF.Form (Html ()) (ExceptT ServantErr IO) ST
    p1 = "title" .: DF.text Nothing

    p2 :: ST -> ExceptT ServantErr IO (Html ())
    p2 title = liftIO $ do
        idea <- (ideaTitle .~ title) <$> generate arbitrary
        runPersist $ addIdea idea
        return . toHtml . Frame $ title

    r :: View (Html ()) -> ST -> ExceptT ServantErr IO (Html ())
    r v formAction = pure . DF.form v formAction $ do
        DF.label "title" v "Title:"
        DF.inputText "title" v
        DF.inputSubmit "create idea"
