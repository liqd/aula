{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module Frontend.Testing
where

import Lucid hiding (href_)
import Servant
import Servant.Missing (throwError500)
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

aulaTesting :: (GenArbitrary m, ActionM m) => ServerT AulaTesting m
aulaTesting =
       (PublicFrame . PageShow <$> Action.aquery getIdeas)
  :<|> (PublicFrame . PageShow <$> Action.aquery getSpaces)
  :<|> (PublicFrame . PageShow <$> Action.aquery getTopics)
  :<|> (PublicFrame . PageShow <$> Action.aquery getUsers)

  :<|> (PageShow <$> mkRandomPassword)
  :<|> undefined
  :<|> throwError500 "testing error500"
  :<|> throwServantErr (err303 { errHeaders = ("Location", "/target") : errHeaders err303 })
  :<|> makeTopicTimeout

data Page404 = Page404

instance Page Page404 where
    isPrivatePage _ = False

instance ToHtml Page404 where
    toHtmlRaw = toHtml
    toHtml Page404 = div_ $ p_ "404"

-- | Make a topic timeout if the timeout is applicable.
makeTopicTimeout :: (ActionPersist m, ActionUserHandler m) => AUID Topic -> m ()
makeTopicTimeout tid = do
    topic <- amquery $ findTopic tid
    case topic ^. topicPhase of
        PhaseRefinement _ -> topicInRefinementTimedOut tid
        PhaseVoting     _ -> topicInVotingTimedOut tid
        _                 -> return ()
