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
import Frontend.Core
import Frontend.Html
import Types


runFrontend :: IO ()
runFrontend = runSettings settings . aulaTweaks $ serve (Proxy :: Proxy FrontendH) frontendH
  where
    settings = setHost (fromString $ Config.config ^. listenerInterface)
             . setPort (Config.config ^. listenerPort)
             $ defaultSettings

-- | FIXME: this should be in moved to a state object that is passed down from 'runFrontend'.
runPersist :: Persist a -> IO a
runPersist = unNat $ unsafePerformIO mkRunPersist

type GetH = Get '[HTML]

type CreateRandom a = "create_random" :> GetH (Frame (ST `Beside` PageShow a))

type FrontendH =
       GetH (Frame ST)
  :<|> "ideas" :> CreateRandom Idea
  :<|> "ideas" :> GetH (Frame PageIdeasOverview)
  :<|> "ideas" :> "create" :> FormH HTML (Html ()) ST
  :<|> "users" :> CreateRandom User
  :<|> "users" :> GetH (Frame (PageShow [User]))
  :<|> "login" :> Capture "login" ST :> GetH (Frame ST)
  :<|> "topics" :> CreateRandom Topic
  :<|> "topics" :> GetH (Frame (PageShow [Topic]))
  :<|> "topics" :> Capture "topic" (AUID Topic) :> GetH (Frame PageTopicOverview)
  :<|> Raw

createRandom :: (MonadIO m, Arbitrary a, Show a) => ST -> AulaLens [a] -> m (Frame (ST `Beside` PageShow a))
createRandom s l = liftIO $ do
    x <- generate arbitrary
    runPersist $ addDb l x
    return (Frame (("new " <> s <> " created.") `Beside` PageShow x))

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
  :<|> createRandom "topic" dbTopics
  :<|> render (PageShow <$> getTopics)
  :<|> pageTopicOverview
  :<|> serveDirectory (Config.config ^. htmlStatic)

pageTopicOverview :: MonadIO m => AUID Topic -> m (Frame PageTopicOverview)
pageTopicOverview topicId = liftIO . runPersist $ do
    -- FIXME 404
    Just topic <- findTopic topicId
    ideas      <- findIdeasByTopic topic
    pure . Frame $ case topic ^. topicPhase of
        PhaseRefinement -> PageTopicOverviewRefinementPhase' $ PageTopicOverviewRefinementPhase topic ideas
        PhaseJury       -> PageTopicOverviewJuryPhase'       $ PageTopicOverviewJuryPhase
        PhaseVoting     -> PageTopicOverviewVotingPhase'     $ PageTopicOverviewVotingPhase
        PhaseResult     -> PageTopicOverviewResultPhase'     $ PageTopicOverviewResultPhase
        -- FIXME: how do we display a topic in the finished phase?
        -- Is this the same the result phase?
        -- Maybe some buttons to hide?
        PhaseFinished   -> PageTopicOverviewResultPhase'     $ PageTopicOverviewResultPhase

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
