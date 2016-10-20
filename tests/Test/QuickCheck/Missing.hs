{-# OPTIONS_GHC -Wall -Werror #-}

module Test.QuickCheck.Missing
where

import System.IO.Unsafe (unsafePerformIO)

import Data.IORef
import Data.Monoid ((<>))
import Test.QuickCheck.Gen

import qualified Data.Set as Set


{-# NOINLINE uniqueOf #-}
uniqueOf :: (Ord a) => Int -> Gen a -> Gen a
uniqueOf numOfTries gen = go numOfTries
  where
    elems = unsafePerformIO $ newIORef Set.empty

    go 0 = error $ "Test.QuickCheck.Missing.unique: Giving up after " <>  show numOfTries <> " tries."
    go n = do
        x <- gen
        let needOtherOne = unsafePerformIO $ do
                xs <- readIORef elems
                if Set.member x xs
                    then pure True
                    else do
                        modifyIORef elems (Set.insert x)
                        pure False
        if needOtherOne
            then go (n - 1)
            else pure x

-- | Generates a list of length between @n@ and @m@.
manyNM :: Int -> Int -> Gen a -> Gen [a]
manyNM n m g = choose (n, m) >>= (`vectorOf` g)
