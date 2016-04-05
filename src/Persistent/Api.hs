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

import Control.Lens
import Control.Monad.Reader (ask)
import Data.Acid hiding (makeAcidic)
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

askDb :: Query AulaData AulaData
askDb = ask

$(makeAcidic ''AulaData
    [ 'askDb, 'setUserEmail, 'setUserRole, 'setTopicPhase
    , 'addIdea
    , 'addTopic
    , 'addUser
    , 'addIdeaResult
    , 'addLikeToIdea
    , 'addVoteToIdea
    , 'addCommentToIdea
    , 'addReplyToIdeaComment
    , 'addCommentVoteToIdeaComment
    , 'addCommentVoteToIdeaCommentReply
    , 'addIdeaSpaceIfNotExists
    , 'editIdea
    , 'saveDurations
    , 'saveQuorums
    ])
