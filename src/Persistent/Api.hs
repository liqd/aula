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
                  { _rpDesc   :: String
                  , _rpQuery  :: m AulaData
                  , _rpUpdate :: forall ev. (UpdateEvent ev, EventState ev ~ AulaData) => ev -> m (EventResult ev)
                  , _rpClose  :: m ()
                  }

makeLenses ''RunPersistT

type RunPersist = RunPersistT IO

askDb :: Query AulaData AulaData
askDb = ask

$(makeAcidic ''AulaData
    [ 'askDb, 'setUserEmail, 'setUserPass, 'setUserRole, 'setUserAvatar, 'setTopicPhase
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
    , 'moveIdeasToLocation
    , 'dangerousResetAulaData
    ])
