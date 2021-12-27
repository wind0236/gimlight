{-# LANGUAGE OverloadedStrings #-}

module Action.DropSpec
    ( spec
    ) where

import           Action                     (ActionResult (ActionResult, killed, newCellMap, status),
                                             ActionStatus (Failed, Ok))
import           Action.Drop                (dropAction)
import           Actor                      (inventoryItems)
import           Control.Lens               ((%~), (&))
import           Control.Monad.State        (execStateT)
import           Control.Monad.Trans.Writer (writer)
import           Data.Either                (fromRight)
import           Dungeon.Map.Cell           (locateActorAt, locateItemAt,
                                             removeActorAt)
import           Inventory                  (removeNthItem)
import           Item                       (getName, herb)
import qualified Localization.Texts         as T
import           SetUp.Dungeon              (initCellMap, initTileCollection,
                                             orcWithFullItemsPosition,
                                             orcWithHerbPosition)
import           Test.Hspec                 (Spec, it, shouldBe)

spec :: Spec
spec = do
    testDropItemSuccessfully
    testItemAlreadyExists

testDropItemSuccessfully :: Spec
testDropItemSuccessfully =
    it "returns a Ok result if there is no item at the player's foot." $
    result `shouldBe` expected
  where
    result = dropAction 0 orcWithHerbPosition initTileCollection initCellMap
    expected = writer (expectedResult, expectedLog)
    expectedResult =
        ActionResult
            {status = Ok, newCellMap = cellMapAfterDropping, killed = []}
    cellMapAfterDropping =
        fromRight undefined $
        flip execStateT initCellMap $ do
            a <- removeActorAt orcWithHerbPosition
            locateActorAt
                initTileCollection
                (a & inventoryItems %~ (snd . removeNthItem 0))
                orcWithHerbPosition
            locateItemAt initTileCollection herb orcWithHerbPosition
    expectedLog = [T.youDropped $ getName herb]

testItemAlreadyExists :: Spec
testItemAlreadyExists =
    it "returns a Failed result if there is already an item at the player's foot." $
    result `shouldBe` expected
  where
    result =
        dropAction 0 orcWithFullItemsPosition initTileCollection initCellMap
    expected = writer (expectedResult, expectedLog)
    expectedResult =
        ActionResult {status = Failed, newCellMap = initCellMap, killed = []}
    expectedLog = [T.itemExists]
