{-# LANGUAGE GADTs #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module Frontend.PageSpec
where

import Arbitrary ()
import Frontend.Page


import Data.Typeable (Typeable, typeOf)
import Test.Hspec (Spec, describe, it, shouldNotBe)
import Test.QuickCheck (Arbitrary(..), Gen, forAll, property)

spec :: Spec
spec = do
    describe "Private and Public page should be inverse" $
        mapM_ publicPagePrivatePageInverseProp [
              P (arb :: Gen PageCreateIdea)
            , P (arb :: Gen PageHomeWithLoginPrompt)
            ]
    where
        arb :: Arbitrary a => Gen a
        arb = arbitrary

data PageGen where
    P :: (Show p, Typeable p, Page p) => Gen p -> PageGen

-- | Check if the public page and private page property are inverse.
publicPagePrivatePageInverseProp :: PageGen -> Spec
publicPagePrivatePageInverseProp (P g) =
    it (show $ typeOf g) . property . forAll g $
        \p -> isPublicPage p `shouldNotBe` isPrivatePage p
