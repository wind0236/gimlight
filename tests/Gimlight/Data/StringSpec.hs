{-# LANGUAGE QuasiQuotes #-}

module Gimlight.Data.StringSpec
  ( spec,
  )
where

import Data.String.QQ (s)
import Gimlight.Data.String (adjustLength, makeTable)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  testAdjustLength
  describe "makeTable" $ do
    testMakeTableWithFilledTable
    testMakeTableContainingEmptyList
    testMakeTableEmptyList
    testMakeTableListOfEmptyList

testAdjustLength :: Spec
testAdjustLength =
  describe "adjustLength" $
    it "appends spaces so that the length is adjusted to the given number." $
      adjustLength 10 "Marion" `shouldBe` "Marion    "

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
