module Action.PickUpSpec
    ( spec
    ) where

import           Action               (ActionResult (ActionResult, killed, newCellMap, status),
                                       ActionStatus (Failed, Ok))
import           Action.PickUp        (pickUpAction)
import           Actor                (inventoryItems, player)
import           Actor.Inventory      (addItem)
import           Control.Lens         ((%~), (&))
import           Control.Monad.Writer (writer)
import           Data.Array           (array)
import           Data.Maybe           (fromJust)
import           Dungeon.Map.Cell     (CellMap, TileIdLayer (TileIdLayer),
                                       cellMap, locateActorAt, locateItemAt,
                                       removeItemAt)
import           Dungeon.Map.Tile     (TileCollection, tile)
import           IndexGenerator       (generator)
import           Item                 (getName, herb)
import           Linear.V2            (V2 (V2))
import qualified Localization.Texts   as T
import           Test.Hspec           (Spec, it, shouldBe)

spec :: Spec
spec = do
    testPickUpSuccess
    testPickUpVoid
    testPickUpWhenInventoryIsFull

testPickUpSuccess :: Spec
testPickUpSuccess =
    it "returns a Ok result if there is an item at the player's foot, and player's inventory is not full." $
    result `shouldBe` expected
  where
    result =
        pickUpAction
            playerPosition
            actorWithoutItem
            initTileCollection
            initCellMap
    expected = writer (expectedResult, expectedLog)
    expectedResult =
        ActionResult
            {status = Ok, newCellMap = cellMapAfterPickingUp, killed = []}
    cellMapAfterPickingUp =
        fromJust $
        removeItemAt playerPosition initCellMap >>=
        locateActorAt actorWithItem playerPosition . snd
    expectedLog = [T.youGotItem $ getName herb]
    actorWithItem =
        actorWithoutItem & inventoryItems %~ (fromJust . addItem herb)
    (actorWithoutItem, _) = player generator
    playerPosition = V2 0 0

testPickUpVoid :: Spec
testPickUpVoid =
    it "returns a Failed result if there is no item at the player's foot." $
    result `shouldBe` expected
  where
    result =
        pickUpAction
            playerPosition
            actorWithoutItem
            initTileCollection
            initCellMap
    expected = writer (expectedResult, expectedLog)
    expectedResult =
        ActionResult
            {status = Failed, newCellMap = cellMapAfterPickingUp, killed = []}
    cellMapAfterPickingUp =
        fromJust $ locateActorAt actorWithoutItem playerPosition initCellMap
    expectedLog = [T.youGotNothing]
    (actorWithoutItem, _) = player generator
    playerPosition = V2 1 0

testPickUpWhenInventoryIsFull :: Spec
testPickUpWhenInventoryIsFull =
    it "returns a Failed result if the player's inventory is full." $
    result `shouldBe` expected
  where
    result =
        pickUpAction
            playerPosition
            actorWithFullItems
            initTileCollection
            initCellMap
    expected = writer (expectedResult, expectedLog)
    expectedResult =
        ActionResult
            {status = Failed, newCellMap = cellMapAfterPickingUp, killed = []}
    cellMapAfterPickingUp =
        fromJust $ locateActorAt actorWithFullItems playerPosition initCellMap
    expectedLog = [T.bagIsFull]
    actorWithFullItems =
        iterate
            (\x -> x & inventoryItems %~ (fromJust . addItem herb))
            (fst $ player generator) !!
        5
    playerPosition = V2 0 0

initCellMap :: CellMap
initCellMap = fromJust $ locateItemAt herb (V2 0 0) cm
  where
    cm =
        cellMap $
        array (V2 0 0, V2 1 0) [(V2 0 0, emptyTile), (V2 1 0, emptyTile)]
    emptyTile = TileIdLayer Nothing Nothing

initTileCollection :: TileCollection
initTileCollection = array (0, 0) [(0, tile True True)]
