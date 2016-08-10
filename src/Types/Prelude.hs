{-# LANGUAGE ConstraintKinds             #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE Rank2Types                  #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeOperators               #-}
{-# LANGUAGE ViewPatterns                #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Types.Prelude
where

import Control.Lens hiding ((<.>))
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Char
import Data.Function (on)
import Data.List as List (sortBy)
import Data.Monoid
import Data.Proxy (Proxy(Proxy))
import Data.SafeCopy (base, deriveSafeCopy)
import Data.String.Conversions
import Data.Time
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import GHC.Generics (Generic)
import Servant ((:~>)(Nat))
import System.Directory (getDirectoryContents)
import Text.Read (readEither)

import qualified Data.Aeson as Aeson
import qualified Data.Text as ST
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

cshow :: (Show a, ConvertibleStrings String c) => a -> c
cshow = cs . show

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

reverseSortOn :: Ord b => Getter a b -> [a] -> [a]
reverseSortOn l = sortBy ((flip compare) `on` view l)

downSortOn :: Ord b => Getter a b -> [a] -> [a]
downSortOn l = sortOn (l . to Data.Ord.Down)

countEq :: (Foldable f, Eq value) => value -> Lens' vote value -> f vote -> Int
countEq v l = lengthOf $ folded . filtered ((== v) . view l)


-- | Use this for storing URLs in the aula state.  Unlike 'UriPath' is serializable, has equality,
-- and unlike "Frontend.Path", it is flexible enough to contain internal and external uris.
-- (FUTUREWORK: the `uri-bytestring` package could be nice here, but it may require a few orphans or
-- a newtype to prevent them; see also: #31.)
type URL = ST


-- * time

newtype Timestamp = Timestamp { unTimestamp :: UTCTime }
  deriving (Eq, Ord, Generic)

timestampToEpoch :: Timestamp -> Integer
timestampToEpoch = round . utcTimeToPOSIXSeconds . unTimestamp

data Timespan =  -- FIXME: import this from thentos?  create a package thentos-base?
    TimespanUs    Integer
  | TimespanMs    Integer
  | TimespanSecs  Integer
  | TimespanMins  Integer
  | TimespanHours Integer
  | TimespanDays  Integer
  deriving (Eq, Ord, Show, Read, Generic)

instance SOP.Generic Timestamp
instance SOP.Generic Timespan

deriveSafeCopy 0 'base ''Timestamp
deriveSafeCopy 0 'base ''Timespan

instance Aeson.ToJSON Timestamp where toJSON = Aeson.gtoJson
instance Aeson.FromJSON Timestamp where parseJSON = Aeson.gparseJson

instance Show Timestamp where
    show = showTimestamp

instance Read Timestamp where
    readsPrec _ s = case splitAt timestampFormatLength $ dropWhile isSpace s of
        (parseTimestamp -> Just t, r) -> [(t, r)]
        _                             -> error $ "Read Timestamp: " <> show s

parseTimestamp :: String -> Maybe Timestamp
parseTimestamp = fmap Timestamp . parseTimeM True defaultTimeLocale timestampFormat

showTimestamp :: Timestamp -> String
showTimestamp = formatTime defaultTimeLocale timestampFormat . unTimestamp

timestampFormat :: String
timestampFormat = "%F_%T_%q"

timestampFormatLength :: Int
timestampFormatLength = length ("1864-04-13_13:01:33_846177415049" :: String)

showTimespan :: Timespan -> String
showTimespan (TimespanUs    i) = show i <> "us"
showTimespan (TimespanMs    i) = show i <> "ms"
showTimespan (TimespanSecs  i) = show i <> "s"
showTimespan (TimespanMins  i) = show i <> "m"
showTimespan (TimespanHours i) = show i <> "h"
showTimespan (TimespanDays  i) = show i <> "d"

timespanUs :: Timespan -> Int
timespanUs (TimespanUs    i) = fromIntegral   i
timespanUs (TimespanMs    i) = fromIntegral $ i * 1000
timespanUs (TimespanSecs  i) = fromIntegral $ i * (1000 * 1000)
timespanUs (TimespanMins  i) = fromIntegral $ i * (1000 * 1000 * 60)
timespanUs (TimespanHours i) = fromIntegral $ i * (1000 * 1000 * 3600)
timespanUs (TimespanDays  i) = fromIntegral $ i * (1000 * 1000 * 3600 * 24)

timespanDays :: Timespan -> Int
timespanDays = (`div` (1000 * 1000 * 3600 * 24)) . timespanUs

instance Aeson.FromJSON Timespan where
    parseJSON = Aeson.withText "Timespan value" $ \raw -> do
        let (digits, units) = ST.break (`notElem` ("-0123456789" :: String)) raw

            bad = fail $ "bad Timespan value: " <> cs (show raw)

            construct :: Monad m => ST -> (Integer -> Timespan) -> m Timespan
            construct i cns = pure . cns . read . cs $ i

        case (digits, units) of
            ("", _)   -> bad
            (i, "us") -> construct i TimespanUs
            (i, "ms") -> construct i TimespanMs
            (i, "s")  -> construct i TimespanSecs
            (i, "m")  -> construct i TimespanMins
            (i, "h")  -> construct i TimespanHours
            (i, "d")  -> construct i TimespanDays
            _         -> bad

instance Aeson.ToJSON Timespan where
    toJSON = \case
        (TimespanUs    i) -> render i "us"
        (TimespanMs    i) -> render i "ms"
        (TimespanSecs  i) -> render i "s"
        (TimespanMins  i) -> render i "m"
        (TimespanHours i) -> render i "h"
        (TimespanDays  i) -> render i "d"
      where
        render :: Integer -> String -> Aeson.Value
        render i unit = Aeson.String . cs $ show i <> unit

diffTimestamps :: Timestamp -> Timestamp -> Timespan
diffTimestamps (Timestamp tfrom) (Timestamp ttill) = TimespanUs .
    round $ (tfrom `diffUTCTime` ttill) * (1000 * 1000)

addTimespan :: Timespan -> Timestamp -> Timestamp
addTimespan tdiff (Timestamp tfrom) = Timestamp $
    fromRational (fromIntegral (timespanUs tdiff) / (1000 * 1000) :: Rational) `addUTCTime` tfrom

fromNow :: Timestamp -> Iso' Timestamp Timespan
fromNow now = iso (`diffTimestamps` now) (`addTimespan` now)


getDirectoryContentsNoDots :: FilePath -> IO [FilePath]
getDirectoryContentsNoDots path = filter (not . (`elem` [".", ".."])) <$> getDirectoryContents path
