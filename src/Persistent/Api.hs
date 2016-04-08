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
import Data.Acid hiding (Query, Update, makeAcidic)
import Data.SafeCopy (base, deriveSafeCopy)

import Persistent.Pure
import Persistent.Idiom
import Persistent.TemplateHaskell (makeAcidic)

import qualified Data.Acid as Acid


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

askDb :: Acid.Query AulaData AulaData
askDb = ask

$(makeAcidic ''AulaData
    -- The only query
    [ 'askDb

    -- AddDb updates
    , 'addCommentToIdea
    , 'addCommentVoteToIdeaComment
    , 'addCommentVoteToIdeaCommentReply
    , 'addDelegation
    , 'addIdea
    , 'addIdeaJuryResult
    , 'addIdeaVoteResult
    , 'addLikeToIdea
    , 'addReplyToIdeaComment
    , 'addTopic
    , 'addUser
    , 'addVoteToIdea

    -- Custom adds
    , 'addFirstUser
    , 'addIdeaSpaceIfNotExists

    -- Custom setters
    , 'dangerousResetAulaData
    , 'saveDurations
    , 'saveQuorums

    -- Needs to be updated to change the meta data
    --   Edit updates
    , 'editIdea
    , 'editTopic
    --   Based on modifyUser:
    , 'setUserRole
    , 'setUserAvatar
    , 'setUserEmail
    --   Based on modifyIdea:
    , 'moveIdeasToLocation
    --   Based on modifyTopic:
    , 'setTopicPhase
    --   Custom:
    , 'setUserPass
    ])
