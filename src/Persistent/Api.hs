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

deriveSafeCopy 0 'base ''IdeaChangedLocation

$(makeAcidic ''AulaData
    [ 'addCommentToIdea
    , 'setCommentDesc
    , 'addCommentVote
    , 'addDelegation
    , 'withdrawDelegation
    , 'addPasswordToken
    , 'removePasswordToken
    , 'addFirstUser
    , 'addIdea
    , 'addIdeaJuryResult
    , 'removeIdeaJuryResult
    , 'setCreatorStatement
    , 'addIdeaSpaceIfNotExists
    , 'addIdeaVoteResult
    , 'revokeWinnerStatus
    , 'addLikeToIdea
    , 'delikeIdea
    , 'addReply
    , 'addTopic
    , 'addTopicYieldLocs
    , 'addUser
    , 'addVoteToIdea
    , 'removeVoteFromIdea
    , 'askDb
    , 'dangerousResetAulaData
    , 'dangerousRenameAllLogins
    , 'editIdea
    , 'deleteIdea
    , 'deleteTopic
    , 'editTopic
    , 'moveIdeasToLocation
    , 'moveIdeaToTopic
    , 'saveAndEnactFreeze
    , 'saveDurations
    , 'saveQuorums
    , 'setTopicPhase
    , 'setUserEmail
    , 'setUserPass
    , 'setUserLogin
    , 'addUserRole
    , 'remUserRole
    , 'setUserDesc
    , 'resetUserPass
    , 'renameClass
    , 'destroyClass
    , 'deleteComment
    , 'deactivateUser
    , 'setTermsOfUse
    , 'wipeIdeasAndTopics
    ])
