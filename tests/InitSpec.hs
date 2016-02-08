module InitSpec where

import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    initSpec

initSpec = describe "Hspec" $
    it "works." $ True `shouldBe` True
