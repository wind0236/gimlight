module Gimlight.Data.EitherSpec
    ( spec
    ) where

import           Control.Exception    (evaluate)
import           Gimlight.Data.Either (expectRight)
import           Test.Hspec           (Spec, describe, errorCall, it, shouldBe,
                                       shouldThrow)

spec :: Spec
spec = testExpectRight

testExpectRight :: Spec
testExpectRight =
    describe "expectRight" $ do
        it "returns the inner value if `Right` is passed." $
            expectRight msg (Right r :: Either String String) `shouldBe` r
        it "panics with the error message if `Left` is passed." $
            evaluate (expectRight msg (Left p)) `shouldThrow`
            errorCall (msg ++ ": " ++ show p)
  where
    r = "Sushi"
    p = "Pizza"
    msg = "Why not sushi!"
