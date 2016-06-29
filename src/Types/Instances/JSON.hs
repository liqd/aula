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

module Types.Instances.JSON
where

import Control.Lens hiding ((<.>))
import Data.List as List (zipWith)
import Data.Map as Map (Map, lookup, unions, singleton)

import qualified Data.Aeson as Aeson
import qualified Data.Vector as Vector
import qualified Generics.Generic.Aeson as Aeson

import Types.Core
import Types.Instances.Optics
import Frontend.Constant


instance Aeson.ToJSON (AUID a) where toJSON = Aeson.gtoJson
instance Aeson.ToJSON CommentKey where toJSON = Aeson.gtoJson
instance Aeson.ToJSON DScope where toJSON = Aeson.gtoJson
instance Aeson.ToJSON Delegation where toJSON = Aeson.gtoJson
instance Aeson.ToJSON EmailAddress where toJSON = Aeson.String . review emailAddress
instance Aeson.ToJSON Freeze where toJSON = Aeson.gtoJson
instance Aeson.ToJSON id => Aeson.ToJSON (GMetaInfo a id) where toJSON = Aeson.gtoJson
instance Aeson.ToJSON IdeaJuryResultType where toJSON = Aeson.gtoJson
instance Aeson.ToJSON IdeaLocation where toJSON = Aeson.gtoJson
instance Aeson.ToJSON IdeaSpace where toJSON = Aeson.gtoJson
instance Aeson.ToJSON IdeaVoteValue where toJSON = Aeson.gtoJson
instance Aeson.ToJSON Phase where toJSON = Aeson.gtoJson
instance Aeson.ToJSON PhaseStatus where toJSON = Aeson.gtoJson
instance Aeson.ToJSON Role where toJSON = Aeson.gtoJson
instance Aeson.ToJSON SchoolClass where toJSON = Aeson.gtoJson
instance Aeson.ToJSON UpDown where toJSON = Aeson.gtoJson
instance Aeson.ToJSON UserFirstName where toJSON = Aeson.gtoJson
instance Aeson.ToJSON UserLastName where toJSON = Aeson.gtoJson
instance Aeson.ToJSON UserLogin where toJSON = Aeson.gtoJson
instance Aeson.ToJSON UserPass where toJSON _ = Aeson.String ""
    -- FIXME: where do we need this?  think of something else!
instance Aeson.ToJSON UserSettings where toJSON = Aeson.gtoJson
instance Aeson.ToJSON User where toJSON = Aeson.gtoJson

instance Aeson.FromJSON (AUID a) where parseJSON = Aeson.gparseJson
instance Aeson.FromJSON CommentKey where parseJSON = Aeson.gparseJson
instance Aeson.FromJSON DScope where parseJSON = Aeson.gparseJson
instance Aeson.FromJSON Delegation where parseJSON = Aeson.gparseJson
instance Aeson.FromJSON EmailAddress where parseJSON = Aeson.withText "email address" $ pure . (^?! emailAddress)
instance Aeson.FromJSON Freeze where parseJSON = Aeson.gparseJson
instance Aeson.FromJSON id => Aeson.FromJSON (GMetaInfo a id) where parseJSON = Aeson.gparseJson
instance Aeson.FromJSON IdeaJuryResultType where parseJSON = Aeson.gparseJson
instance Aeson.FromJSON IdeaLocation where parseJSON = Aeson.gparseJson
instance Aeson.FromJSON IdeaSpace where parseJSON = Aeson.gparseJson
instance Aeson.FromJSON IdeaVoteValue where parseJSON = Aeson.gparseJson
instance Aeson.FromJSON Phase where parseJSON = Aeson.gparseJson
instance Aeson.FromJSON PhaseStatus where parseJSON = Aeson.gparseJson
instance Aeson.FromJSON Role where parseJSON = Aeson.gparseJson
instance Aeson.FromJSON SchoolClass where parseJSON = Aeson.gparseJson
instance Aeson.FromJSON UpDown where parseJSON = Aeson.gparseJson
instance Aeson.FromJSON UserFirstName where parseJSON = Aeson.gparseJson
instance Aeson.FromJSON UserLastName where parseJSON = Aeson.gparseJson
instance Aeson.FromJSON UserLogin where parseJSON = Aeson.gparseJson
instance Aeson.FromJSON UserPass where parseJSON _ = pure . UserPassInitial $ InitialPassword ""
    -- FIXME: where do we need this?  think of something else!
instance Aeson.FromJSON UserSettings where parseJSON = Aeson.gparseJson
instance Aeson.FromJSON User where parseJSON = Aeson.gparseJson


instance Aeson.ToJSON DelegationNetwork where
    toJSON (DelegationNetwork nodes links) = result
      where
        result = Aeson.object
            [ "nodes" Aeson..= array (renderNode <$> nodes)
            , "links" Aeson..= array (renderLink <$> links)
            ]

        -- FIXME: It shouldn't be rendered for deleted users.
        renderNode (u, p) = Aeson.object
            [ "name"   Aeson..= (u ^. userLogin . unUserLogin)
            , "avatar" Aeson..= (u ^. userAvatar avatarDefaultSize)
            , "power"  Aeson..= p
            ]

        renderLink (Delegation _ u1 u2) = Aeson.object
            [ "source"  Aeson..= nodeId u1
            , "target"  Aeson..= nodeId u2
            ]

        -- the d3 edges refer to nodes by list position, not name.  this function gives the list
        -- position.
        nodeId :: AUID User -> Aeson.Value
        nodeId uid = Aeson.toJSON . (\(Just pos) -> pos) $ Map.lookup uid m
          where
            m :: Map.Map (AUID User) Int
            m = Map.unions $ List.zipWith f nodes [0..]

            f :: (User, Int) -> Int -> Map.Map (AUID User) Int
            f (u, _) = Map.singleton (u ^. to _userMeta . to _metaKey)

        array :: Aeson.ToJSON v => [v] -> Aeson.Value
        array = Aeson.Array . Vector.fromList . fmap Aeson.toJSON
