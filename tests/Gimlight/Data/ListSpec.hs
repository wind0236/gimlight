module Gimlight.Data.ListSpec
    ( spec
    ) where

import           Gimlight.Data.List (intercalateIncludingHeadTail)
import           Test.Hspec         (Spec, describe, it, shouldBe)

spec :: Spec
spec = testIntercalateIncludingHeadTail

testIntercalateIncludingHeadTail :: Spec
testIntercalateIncludingHeadTail =
    describe "intercalateIncludingHeadTail" $
    it "inserts a list between lists of the second argument, append it, prepend it, and concat them." $
    intercalateIncludingHeadTail "|" ["Ester", "Menyahnya", "Shinobu"] `shouldBe`
    "|Ester|Menyahnya|Shinobu|"
