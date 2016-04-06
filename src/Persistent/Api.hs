{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE TypeFamilies                #-}
{-# LANGUAGE ConstraintKinds             #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GADTs                       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE ImpredicativeTypes          #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TupleSections               #-}
{-# LANGUAGE TypeOperators               #-}
{-# LANGUAGE ViewPatterns                #-}

{-# OPTIONS_GHC -Wall -Werror -fno-warn-orphans #-}

module Persistent.Api
{-
    ( RunPersistT(..), rpDesc, rpState, rpClose
    , RunPersist
    , module Persistent.Pure
    , module Persistent.Idiom
    )
-}
where

import Control.Exception (finally)
import Control.Lens
import Control.Monad.Reader (ask)
import Data.Acid hiding (makeAcidic)
import Data.Monoid
import Data.SafeCopy (base, deriveSafeCopy)

import Persistent.Pure
import Persistent.Idiom
import Persistent.TemplateHaskell (makeAcidic)


deriveSafeCopy 0 'base ''AulaData

data RunPersistT m =
        RunPersist
                  { _rpDesc  :: String
                  , _rpState :: AcidState AulaData
                  , _rpClose :: m ()
                  }

makeLenses ''RunPersistT

type RunPersist = RunPersistT IO

-- | A more low-level variant of 'Persistent.Implementation.withPersist' with the implementation
-- explicit as parameter.
--
-- TODO: move to "Persistent.Implementation"?  or move both @withPersist*@s here?
withPersist' :: IO RunPersist -> (RunPersist -> IO a) -> IO a
withPersist' mkRunP m = do
    rp@(RunPersist desc _ close) <- mkRunP  -- initialization happens here
    putStrLn $ "persistence: " <> desc -- FIXME: use logger for this (or perhaps log in the construction of Action, where we have a logger?)
    m rp `finally` close  -- closing happens here

askDb :: Query AulaData AulaData
askDb = ask

$(makeAcidic ''AulaData
    [ 'askDb, 'setUserEmail, 'setUserPass, 'setUserRole, 'setTopicPhase
    , 'addIdea
    , 'addTopic
    , 'addUser
    , 'addIdeaJuryResult
    , 'addIdeaVoteResult
    , 'addFirstUser
    , 'addDelegation
    , 'addLikeToIdea
    , 'addVoteToIdea
    , 'addCommentToIdea
    , 'addReplyToIdeaComment
    , 'addCommentVoteToIdeaComment
    , 'addCommentVoteToIdeaCommentReply
    , 'addIdeaSpaceIfNotExists
    , 'editIdea
    , 'editTopic
    , 'saveDurations
    , 'saveQuorums
    ])
