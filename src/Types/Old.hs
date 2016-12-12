{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Types.Old where

import Types.Core
import Data.String.Conversions (ST)
import Data.Data     (Data)
import Data.Typeable (Typeable)
import GHC.Generics  (Generic)

data SchoolClass_V0 = SchoolClass_V0 Int ST
  deriving (Eq, Ord, Show, Read, Generic, Typeable, Data)

data Role_V0 =
    Student_V0    SchoolClass
  | ClassGuest_V0 SchoolClass -- ^ e.g., parents
  | SchoolGuest_V0  -- ^ e.g., researchers
  | Moderator_V0
  | Principal_V0
  | Admin_V0
  deriving (Eq, Ord, Show, Read, Generic, Typeable, Data)
