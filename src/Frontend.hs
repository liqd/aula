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

render :: MonadIO m => Persist body -> m (Frame body)
render m = liftIO . runPersist $ Frame <$> m

frontendH :: Server FrontendH
frontendH =
       return (Frame "yihaah!")
  :<|> createRandom "idea" dbIdeas
  :<|> render (PageIdeasOverview <$> getIdeas)
  :<|> Page.createIdea
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
