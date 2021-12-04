{-# LANGUAGE OverloadedStrings #-}

module Action.DropSpec
    ( spec
    ) where

import           Action                     (ActionResult (ActionResult, killed, newDungeon, status),
                                             ActionStatus (Failed, Ok))
import           Action.Drop                (dropAction)
import           Actor                      (Actor, inventoryItems, player)
import           Actor.Inventory            (addItem)
import           Control.Lens               ((&), (.~), (^.))
import           Control.Monad.Trans.Writer (writer)
import           Data.Array                 (array)
import           Data.Maybe                 (fromJust)
import           Dungeon                    (Dungeon, dungeon, pushActor,
                                             pushItem)
import           Dungeon.Identifier         (Identifier (Beaeve))
import           Dungeon.Map.Cell           (TileIdLayer (TileIdLayer), cellMap)
import           Dungeon.Map.Tile           (TileCollection, tile)
import           IndexGenerator             (generator)
import           Item                       (getName, herb)
import           Linear.V2                  (V2 (V2))
import qualified Localization.Texts         as T
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
    result =
        dropAction 0 playerPosition actorWithItem initTileCollection initDungeon
    expected = writer (expectedResult, expectedLog)
    expectedResult =
        ActionResult
            {status = Ok, newDungeon = dungeonAfterDropping, killed = []}
    dungeonAfterDropping =
        pushItem playerPosition herb $
        pushActor playerPosition actorWithoutItem initDungeon
    expectedLog = [T.youDropped $ getName herb]
    (actorWithoutItem, actorWithItem) = playerWithoutAndWithItem
    playerPosition = V2 1 0

testItemAlreadyExists :: Spec
testItemAlreadyExists =
    it "returns a Failed result if there is already an item at the player's foot." $
    result `shouldBe` expected
  where
    result =
        dropAction 0 playerPosition actorWithItem initTileCollection initDungeon
    expected = writer (expectedResult, expectedLog)
    expectedResult =
        ActionResult
            {status = Failed, newDungeon = dungeonAfterAction, killed = []}
    dungeonAfterAction = pushActor playerPosition actorWithItem initDungeon
    expectedLog = [T.itemExists]
    (_, actorWithItem) = playerWithoutAndWithItem
    playerPosition = V2 0 0

initDungeon :: Dungeon
initDungeon = pushItem (V2 0 0) herb $ dungeon cm Beaeve
  where
    cm =
        cellMap $
        array (V2 0 0, V2 1 0) [(V2 0 0, emptyTile), (V2 1 0, emptyTile)]
    emptyTile = TileIdLayer Nothing Nothing

playerWithoutAndWithItem :: (Actor, Actor)
playerWithoutAndWithItem = (without, with)
  where
    without = fst $ player generator
    with =
        (\x -> without & inventoryItems .~ x)
            (fromJust $ addItem herb (without ^. inventoryItems))

initTileCollection :: TileCollection
initTileCollection = array (0, 0) [(0, tile True True)]
