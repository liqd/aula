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

{-# OPTIONS_GHC -Wall -Werror #-}

module Types.Instances.Optics
where

import Control.Lens hiding ((<.>))
import Data.String.Conversions

import qualified Text.Email.Validate as Email

import Data.Markdown
import Types.Core
import Types.Prelude


makePrisms ''AUID
makePrisms ''Category
makePrisms ''PlainDocument
makePrisms ''DScope
makePrisms ''Document
makePrisms ''EmailAddress
makePrisms ''IdeaJuryResultValue
makePrisms ''IdeaLocation
makePrisms ''IdeaSpace
makePrisms ''IdeaVoteResultValue
makePrisms ''IdeaVoteValue
makePrisms ''Freeze
makePrisms ''PhaseStatus
makePrisms ''Phase
makePrisms ''Role
makePrisms ''Timestamp
makePrisms ''UpDown
makePrisms ''UserFirstName
makePrisms ''UserLastName
makePrisms ''UserLogin
makePrisms ''UserPass
makePrisms ''UserView

makeLenses ''AUID
makeLenses ''Category
makeLenses ''Comment
makeLenses ''CommentContext
makeLenses ''CommentKey
makeLenses ''CommentVote
makeLenses ''CommentVoteKey
makeLenses ''Delegation
makeLenses ''DelegationNetwork
makeLenses ''DScope
makeLenses ''EmailAddress
makeLenses ''EncryptedPassword
makeLenses ''Freeze
makeLenses ''GMetaInfo
makeLenses ''Idea
makeLenses ''IdeaJuryResult
makeLenses ''IdeaLike
makeLenses ''IdeaLocation
makeLenses ''IdeaSpace
makeLenses ''IdeaVote
makeLenses ''IdeaVoteLikeKey
makeLenses ''IdeaVoteResult
makeLenses ''InitialPassword
makeLenses ''Phase
makeLenses ''PhaseStatus
makeLenses ''PlainDocument
makeLenses ''ProtoIdea
makeLenses ''ProtoIdeaLike
makeLenses ''ProtoIdeaVote
makeLenses ''ProtoTopic
makeLenses ''ProtoUser
makeLenses ''Role
makeLenses ''SchoolClass
makeLenses ''Topic
makeLenses ''UpDown
makeLenses ''User
makeLenses ''UserFirstName
makeLenses ''UserLastName
makeLenses ''UserLogin
makeLenses ''UserPass
makeLenses ''UserSettings
makeLenses ''UserView

-- | Examples:
--
-- >>>    e :: EmailAddress
-- >>>    s :: ST
-- >>>    s = emailAddress # e
-- >>>
-- >>>    s :: ST
-- >>>    s = "foo@example.com"
-- >>>    e :: Maybe EmailAddress
-- >>>    e = s ^? emailAddress
--
-- These more limited type signatures are also valid:
--
-- >>>    emailAddress :: Prism' ST  EmailAddress
-- >>>    emailAddress :: Prism' LBS EmailAddress
emailAddress :: (CSI s t SBS SBS) => Prism s t EmailAddress EmailAddress
emailAddress = csi . prism' Email.toByteString Email.emailAddress . from _InternalEmailAddress
