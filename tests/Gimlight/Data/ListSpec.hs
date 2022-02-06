module Gimlight.Data.ListSpec
    ( spec
    ) where

import           Gimlight.Data.List (filterAll, intercalateIncludingHeadTail)
import           Test.Hspec         (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    testIntercalateIncludingHeadTail
    testFilterAll

testIntercalateIncludingHeadTail :: Spec
testIntercalateIncludingHeadTail =
    describe "intercalateIncludingHeadTail" $
    it "inserts a list between lists of the second argument, append it, prepend it, and concat them." $
    intercalateIncludingHeadTail "|" ["Ester", "Menyahnya", "Shinobu"] `shouldBe`
    "|Ester|Menyahnya|Shinobu|"

testFilterAll :: Spec
testFilterAll =
    describe "filterAll" $
    it "filters with functions in a list" $
    filterAll [(> 3), (< 6)] [1 :: Int .. 10] `shouldBe` [4, 5]
