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

module Types.Instances.SafeCopy
where

import Control.Monad
import Data.SafeCopy (base, SafeCopy(..), safeGet, safePut, contain, deriveSafeCopy)

import qualified Text.Email.Validate as Email

import Types.Core


deriveSafeCopy 0 'base ''AUID
deriveSafeCopy 0 'base ''Category
deriveSafeCopy 0 'base ''ClassName
deriveSafeCopy 0 'base ''Comment
deriveSafeCopy 0 'base ''CommentContent
deriveSafeCopy 0 'base ''CommentKey
deriveSafeCopy 0 'base ''CommentVote
deriveSafeCopy 0 'base ''CommentVoteKey
deriveSafeCopy 0 'base ''Delegation
deriveSafeCopy 0 'base ''DelegationNetwork
deriveSafeCopy 0 'base ''DScope
deriveSafeCopy 0 'base ''DurationDays
deriveSafeCopy 0 'base ''EncryptedPassword
deriveSafeCopy 0 'base ''Freeze
deriveSafeCopy 0 'base ''GMetaInfo
deriveSafeCopy 0 'base ''Idea
deriveSafeCopy 0 'base ''IdeaJuryResult
deriveSafeCopy 0 'base ''IdeaJuryResultValue
deriveSafeCopy 0 'base ''IdeaLike
deriveSafeCopy 0 'base ''IdeaLikeValue
deriveSafeCopy 0 'base ''IdeaLocation
deriveSafeCopy 0 'base ''IdeaSpace
deriveSafeCopy 0 'base ''IdeaVote
deriveSafeCopy 0 'base ''IdeaVoteLikeKey
deriveSafeCopy 0 'base ''IdeaVoteResult
deriveSafeCopy 0 'base ''IdeaVoteResultValue
deriveSafeCopy 0 'base ''IdeaVoteValue
deriveSafeCopy 0 'base ''InitialPassword
deriveSafeCopy 0 'base ''Phase
deriveSafeCopy 0 'base ''PhaseStatus
deriveSafeCopy 0 'base ''ProtoIdea
deriveSafeCopy 0 'base ''ProtoIdeaLike
deriveSafeCopy 0 'base ''ProtoIdeaVote
deriveSafeCopy 0 'base ''ProtoTopic
deriveSafeCopy 0 'base ''ProtoUser
deriveSafeCopy 0 'base ''Role
deriveSafeCopy 0 'base ''SchoolClass
deriveSafeCopy 0 'base ''Topic
deriveSafeCopy 0 'base ''UpDown
deriveSafeCopy 0 'base ''User
deriveSafeCopy 0 'base ''UserFirstName
deriveSafeCopy 0 'base ''UserLastName
deriveSafeCopy 0 'base ''UserLogin
deriveSafeCopy 0 'base ''UserPass
deriveSafeCopy 0 'base ''UserSettings

instance SafeCopy EmailAddress where
    kind = base
    getCopy = contain $ maybe mzero (pure . InternalEmailAddress) . Email.emailAddress =<< safeGet
    putCopy = contain . safePut . Email.toByteString . internalEmailAddress
