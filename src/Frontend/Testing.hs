{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module Frontend.Testing
where

import Lucid hiding (href_)
import Servant
import Servant.Missing (redirect, throwError500)
import Thentos.Prelude

import Frontend.Core
import Persistent
import Types
import Action


type AulaTesting =
       "ideas"  :> GetH (Frame (PageShow [Idea]))
  :<|> "spaces" :> GetH (Frame (PageShow [IdeaSpace]))
  :<|> "topics" :> GetH (Frame (PageShow [Topic]))
  :<|> "users"  :> GetH (Frame (PageShow [User]))

  :<|> "random-password" :> GetH (PageShow UserPass)
  :<|> "undefined" :> GetH ()
  :<|> "error500" :> GetH ()
  :<|> "error303" :> GetH ()
  :<|> "topic" :> Capture "topic" (AUID Topic) :> "timeout" :> GetH ()
  :<|> "topic" :> Capture "topic" (AUID Topic) :> "next-phase" :> GetH ()

aulaTesting :: (GenArbitrary m, ActionM m) => ServerT AulaTesting m
aulaTesting =
       (PublicFrame . PageShow <$> Action.query getIdeas)
  :<|> (PublicFrame . PageShow <$> Action.query getSpaces)
  :<|> (PublicFrame . PageShow <$> Action.query getTopics)
  :<|> (PublicFrame . PageShow <$> Action.query getAllUsers)

  :<|> (PageShow <$> mkRandomPassword)
  :<|> undefined  -- (intentional)
  :<|> throwError500 "testing error500"
  :<|> throwServantErr (err303 { errHeaders = ("Location", "/target") : errHeaders err303 })
  :<|> (\tid -> Servant.Missing.redirect $ "/testing/topic/"
          <> show (fromIntegral tid :: Int) <> "/next-phase")  -- FIXME: deprecated!  remove!
  :<|> topicForceNextPhase

data Page404 = Page404

instance Page Page404 where
    isPrivatePage _ = False

instance ToHtml Page404 where
    toHtmlRaw = toHtml
    toHtml Page404 = div_ $ p_ "404"

-- | Make a topic timeout if the timeout is applicable.
topicForceNextPhase :: (ActionPersist m, ActionUserHandler m, ActionSendMail m, ActionCurrentTimestamp m)
      => AUID Topic -> m ()
topicForceNextPhase tid = do
    topic <- mquery $ findTopic tid
    case topic ^. topicPhase of
        PhaseRefinement _ -> topicInRefinementTimedOut tid
        PhaseJury         -> makeEverythingFeasible topic
        PhaseVoting     _ -> topicInVotingTimedOut tid
        PhaseResult       -> throwError500 "No phase after result phase!"

makeEverythingFeasible :: (ActionPersist m, ActionUserHandler m, ActionCurrentTimestamp m, ActionSendMail m)
      => Topic -> m ()
makeEverythingFeasible topic = do
    loginByName "admin"
    ideas :: [Idea] <- query $ findIdeasByTopic topic
    (\idea -> markIdeaInJuryPhase (idea ^. _Id) (Feasible Nothing)) `mapM_` ideas
