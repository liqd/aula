{-# LANGUAGE ConstraintKinds             #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE Rank2Types                  #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE TypeOperators               #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Types.Prelude
where

import Control.Lens hiding ((<.>))
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Char
import Data.Function (on)
import Data.List as List (sortBy)
import Data.Proxy (Proxy(Proxy))
import Data.String.Conversions
import Data.Time
import GHC.Generics (Generic)
import Servant ((:~>)(Nat))
import Text.Read (readEither)

import qualified Data.Aeson as Aeson
import qualified Data.Ord (Down(Down))
import qualified Generics.Generic.Aeson as Aeson
import qualified Generics.SOP as SOP


-- | A shorter alias for 'mempty'.
nil :: Monoid a => a
nil = mempty

isNil :: (Monoid a, Eq a) => a -> Bool
isNil = (== nil)

readWith :: Read a => Proxy a -> String -> a
readWith Proxy = read

justIf :: a -> Bool -> Maybe a
justIf x b = if b then Just x else Nothing

justIfP :: a -> (a -> Bool) -> Maybe a
justIfP x f = justIf x (f x)

lowerFirst :: String -> String
lowerFirst [] = []
lowerFirst (x:xs) = toLower x : xs

toEnumMay :: forall a. (Enum a, Bounded a) => Int -> Maybe a
toEnumMay i = if i >= 0 && i <= fromEnum (maxBound :: a)
    then Just $ toEnum i
    else Nothing

readEitherCS :: (ConvertibleStrings String c, Read a) => String -> Either c a
readEitherCS = either (Left . cs) Right . readEither

type CSI s t a b = (ConvertibleStrings s a, ConvertibleStrings b t)
type CSI' s a = CSI s s a a

-- An optic for string conversion
-- let p = ("a" :: ST, Just ("b" :: SBS))
-- p ^. _1 . csi :: SBS
-- > "a"
-- p & _1 . csi %~ ('x':)
-- > ("xa", "b")
csi :: CSI s t a b => Iso s t a b
csi = iso cs cs

showed :: Show a => Getter a String
showed = to show

_utctDay :: Lens' UTCTime Day
_utctDay f t = (\d -> t { utctDay = d }) <$> f (utctDay t)

-- As in the lens-datetime package
julianDay :: Iso' Day Integer
julianDay = iso toModifiedJulianDay ModifiedJulianDay

exceptToFail :: (Monad m, Show e) => ExceptT e m :~> m
exceptToFail = Nat ((either (fail . show) pure =<<) . runExceptT)

data Either3 a b c = Left3 a | Middle3 b | Right3 c
  deriving (Eq, Ord, Show, Read, Generic)

instance (SOP.Generic a, SOP.Generic b, SOP.Generic c) => SOP.Generic (Either3 a b c)

instance (Aeson.ToJSON a, Aeson.ToJSON b, Aeson.ToJSON c) => Aeson.ToJSON (Either3 a b c) where toJSON = Aeson.gtoJson
instance (Aeson.FromJSON a, Aeson.FromJSON b, Aeson.FromJSON c) => Aeson.FromJSON (Either3 a b c) where parseJSON = Aeson.gparseJson

infixr 9 <..>

(<..>) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(<..>) f g x y = f $ g x y

infixr 9 <...>

(<...>) :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
(<...>) f g x y z = f $ g x y z

sortOn :: Ord b => Getter a b -> [a] -> [a]
sortOn l = sortBy (compare `on` view l)

downSortOn :: Ord b => Getter a b -> [a] -> [a]
downSortOn l = sortOn (l . to Data.Ord.Down)
