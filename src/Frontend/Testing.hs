{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module Frontend.Testing
where

import Servant
import Servant.Missing (throwError500)
import Thentos.Prelude

import CreateRandom
import Frontend.Core
import Persistent
import Types
import Action
import Action.Implementation


type AulaTesting =
       "idea"  :> CreateRandom Idea
  :<|> "space" :> CreateRandom IdeaSpace
  :<|> "topic" :> CreateRandom Topic

  :<|> "ideas"  :> GetH (Frame (PageShow [Idea]))
  :<|> "spaces" :> GetH (Frame (PageShow [IdeaSpace]))
  :<|> "topics" :> GetH (Frame (PageShow [Topic]))
  :<|> "users"  :> GetH (Frame (PageShow [User]))

  :<|> "random-password" :> GetH (PageShow UserPass)
  :<|> "undefined" :> GetH ()
  :<|> "error500" :> GetH ()
  :<|> "error303" :> GetH ()
  :<|> "topic" :> Capture "topic" (AUID Topic) :> "timeout" :> GetH ()

aulaTesting :: (GenArbitrary r, PersistM r) => ServerT AulaTesting (Action r)
aulaTesting =
       createRandom DbIdeas
  :<|> createRandomNoMeta DbSpaceSet
  :<|> createRandom DbTopics

  :<|> (PublicFrame . PageShow <$> Action.persistent getIdeas)
  :<|> (PublicFrame . PageShow <$> Action.persistent getSpaces)
  :<|> (PublicFrame . PageShow <$> Action.persistent getTopics)
  :<|> (PublicFrame . PageShow <$> Action.persistent getUsers)

  :<|> (PageShow <$> Action.persistent mkRandomPassword)
  :<|> undefined
  :<|> throwError500 "testing error500"
  :<|> throwServantErr (err303 { errHeaders = ("Location", "/target") : errHeaders err303 })
  :<|> makeTopicTimeout

-- | Make a topic timeout if the timeout is applicable.
makeTopicTimeout :: (ActionPersist r m, ActionUserHandler m) => AUID Topic -> m ()
makeTopicTimeout tid = do
    Just topic <- persistent $ findTopic tid -- FIXME: 404
    case topic ^. topicPhase of
        PhaseRefinement _ -> topicInRefinementTimedOut tid
        PhaseVoting     _ -> topicInVotingTimedOut tid
        _                 -> return ()
