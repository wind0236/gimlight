{-# LANGUAGE QuasiQuotes #-}

module Gimlight.Data.ListSpec
    ( spec
    ) where

import           Data.String.QQ     (s)
import           Gimlight.Data.List (intercalateIncludingHeadTail, makeTable)
import           Test.Hspec         (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    testIntercalateIncludingHeadTail
    describe "makeTable" $ do
        testMakeTableWithFilledTable
        testMakeTableContainingEmptyList
        testMakeTableEmptyList
        testMakeTableListOfEmptyList

testIntercalateIncludingHeadTail :: Spec
testIntercalateIncludingHeadTail =
    describe "intercalateIncludingHeadTail" $
    it "inserts a list between lists of the second argument, append it, prepend it, and concat them." $
    intercalateIncludingHeadTail "|" ["Ester", "Menyahnya", "Shinobu"] `shouldBe`
    "|Ester|Menyahnya|Shinobu|"

testMakeTableWithFilledTable :: Spec
testMakeTableWithFilledTable =
    it "fills all cells if the all rows have the same length." $
    result `shouldBe` expected
  where
    result =
        makeTable
            [["Derich", "Rosemary", "Beroberos"], ["Hapico", "Mussle", "Fuku"]]
    expected =
        [s|
+---------+---------+---------+
|Derich   |Rosemary |Beroberos|
+---------+---------+---------+
|Hapico   |Mussle   |Fuku     |
+---------+---------+---------+|]

testMakeTableContainingEmptyList :: Spec
testMakeTableContainingEmptyList =
    it "leaves cells as blank if a list is shorter than the longest one." $
    result `shouldBe` expected
  where
    result = makeTable [[], ["Foo", "Bar"], ["Baz"]]
    expected =
        [s|
+---+---+
|   |   |
+---+---+
|Foo|Bar|
+---+---+
|Baz|   |
+---+---+|]

testMakeTableEmptyList :: Spec
testMakeTableEmptyList =
    it "returns + if an empty list is passed." $ makeTable [] `shouldBe` "+"

testMakeTableListOfEmptyList :: Spec
testMakeTableListOfEmptyList =
    it "returns a vertical list if an list of empty lists is passed." $
    makeTable [[], [], []] `shouldBe` expected
  where
    expected =
        [s|
++
||
++
||
++
||
++|]
