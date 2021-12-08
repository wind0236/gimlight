{-# LANGUAGE OverloadedStrings #-}

module Action.MeleeSpec
    ( spec
    ) where

import           Action               (ActionResult (ActionResult, killed, newCellMap, status),
                                       ActionStatus (Ok))
import           Action.Melee         (meleeAction)
import           Actor                (attackFromTo)
import           Control.Monad.Writer (runWriter, writer)
import           Data.Maybe           (fromJust)
import           Dungeon.Map.Cell     (locateActorAt, removeActorAt)
import           SetUp                (initCellMap, initTileCollection,
                                       intermediateOrcPosition,
                                       strongestOrcPosition, weakestOrcPosition)
import           Test.Hspec           (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    testKill
    testDamage

testKill :: Spec
testKill =
    describe "Strongest orc" $ do
        it "kills the weakest orc" $ result `shouldBe` expected
        it "returns a Nothing defender" $ newDefender `shouldBe` Nothing
  where
    result =
        meleeAction offset strongestOrcPosition initTileCollection initCellMap
    expected = writer (expectedResult, expectedLog)
    expectedResult =
        ActionResult
            { status = Ok
            , newCellMap = cellMapWithoutDefender
            , killed = [defender]
            }
    ((_, newDefender), expectedLog) = runWriter $ attackFromTo attacker defender
    (defender, cellMapWithoutDefender) =
        fromJust $ removeActorAt weakestOrcPosition initCellMap
    attacker = fst $ fromJust $ removeActorAt strongestOrcPosition initCellMap
    offset = weakestOrcPosition - strongestOrcPosition

testDamage :: Spec
testDamage =
    describe "Strongest orc" $
    it "attacks to the intermediate orc" $ result `shouldBe` expected
  where
    result =
        meleeAction offset strongestOrcPosition initTileCollection initCellMap
    expected = writer (expectedResult, expectedLog)
    expectedResult =
        ActionResult
            { status = Ok
            , newCellMap =
                  fromJust $
                  locateActorAt
                      (fromJust newDefender)
                      intermediateOrcPosition
                      cellMapWithoutDefender
            , killed = []
            }
    ((_, newDefender), expectedLog) = runWriter $ attackFromTo attacker defender
    (defender, cellMapWithoutDefender) =
        fromJust $ removeActorAt intermediateOrcPosition initCellMap
    attacker = fst $ fromJust $ removeActorAt strongestOrcPosition initCellMap
    offset = intermediateOrcPosition - strongestOrcPosition
