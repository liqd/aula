{-# LANGUAGE ConstraintKinds             #-}
{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DefaultSignatures           #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE KindSignatures              #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE MultiParamTypeClasses       #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE Rank2Types                  #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}
{-# LANGUAGE TypeOperators               #-}
{-# LANGUAGE TypeSynonymInstances        #-}
{-# LANGUAGE ViewPatterns                #-}

{-# OPTIONS_GHC -Wall -Werror -fno-warn-orphans #-}

module Types.Instances.SOP
where

import qualified Generics.SOP as SOP

import Types.Core


instance SOP.Generic (AUID a)
instance SOP.Generic Category
instance SOP.Generic Comment
instance SOP.Generic CommentContent
instance SOP.Generic CommentContext
instance SOP.Generic CommentKey
instance SOP.Generic CommentVote
instance SOP.Generic CommentVoteKey
instance SOP.Generic Delegation
instance SOP.Generic DelegationNetwork
instance SOP.Generic DScope
instance SOP.Generic DScopeFull
instance SOP.Generic DurationDays
instance SOP.Generic EncryptedPassword
instance SOP.Generic Freeze
instance SOP.Generic id => SOP.Generic (GMetaInfo a id)
instance SOP.Generic Idea
instance SOP.Generic IdeaJuryResult
instance SOP.Generic IdeaJuryResultType
instance SOP.Generic IdeaJuryResultValue
instance SOP.Generic IdeaLike
instance SOP.Generic IdeaLocation
instance SOP.Generic IdeaSpace
instance SOP.Generic IdeaVote
instance SOP.Generic IdeaVoteLikeKey
instance SOP.Generic IdeaVoteResult
instance SOP.Generic IdeaVoteResultValue
instance SOP.Generic IdeaVoteValue
instance SOP.Generic InitialPassword
instance SOP.Generic Phase
instance SOP.Generic PhaseStatus
instance SOP.Generic ProtoIdea
instance SOP.Generic ProtoIdeaVote
instance SOP.Generic ProtoTopic
instance SOP.Generic ProtoUser
instance SOP.Generic Role
instance SOP.Generic Topic
instance SOP.Generic UpDown
instance SOP.Generic User
instance SOP.Generic UserPass
instance SOP.Generic UserSettings
