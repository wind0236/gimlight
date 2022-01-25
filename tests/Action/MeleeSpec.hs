{-# LANGUAGE OverloadedStrings #-}

module Action.MeleeSpec
    ( spec
    ) where

import           Action               (ActionResult (ActionResult, killed, newCellMap, status),
                                       ActionStatus (Ok))
import           Action.Melee         (meleeAction)
import           Actor                (attackFromTo)
import           Control.Monad.State  (StateT (runStateT), evalStateT,
                                       execStateT)
import           Control.Monad.Writer (runWriter, writer)
import           Data.Either          (fromRight)
import           Data.Maybe           (fromJust)
import           Dungeon.Map.Cell     (locateActorAt, removeActorAt)
import           SetUp.CellMap        (initCellMap, initTileCollection,
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
        fromRight undefined $
        flip runStateT initCellMap $ removeActorAt weakestOrcPosition
    attacker =
        fromRight undefined $
        flip evalStateT initCellMap $ removeActorAt strongestOrcPosition
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
                  fromRight undefined $
                  flip execStateT cellMapWithoutDefender $
                  locateActorAt
                      initTileCollection
                      (fromJust newDefender)
                      intermediateOrcPosition
            , killed = []
            }
    ((_, newDefender), expectedLog) = runWriter $ attackFromTo attacker defender
    (defender, cellMapWithoutDefender) =
        fromRight undefined $
        flip runStateT initCellMap $ removeActorAt intermediateOrcPosition
    attacker =
        fromRight undefined $
        flip evalStateT initCellMap $ removeActorAt strongestOrcPosition
    offset = intermediateOrcPosition - strongestOrcPosition
