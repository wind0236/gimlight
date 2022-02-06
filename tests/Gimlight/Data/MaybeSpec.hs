module Gimlight.Data.MaybeSpec
    ( spec
    ) where

import           Control.Exception   (evaluate)
import           Gimlight.Data.Maybe (expectJust)
import           Test.Hspec          (Spec, describe, errorCall, it, shouldBe,
                                      shouldThrow)

spec :: Spec
spec = testExpectJust

testExpectJust :: Spec
testExpectJust =
    describe "expectJust" $ do
        it "returns the inner value if it receives a `Just` value." $
            expectJust msg (Just v) `shouldBe` v
        it "panics if it receives a `Nothing` value." $
            evaluate (expectJust msg Nothing) `shouldThrow` errorCall msg
  where
    v = "Marion"
    msg = "We need a Marion."
