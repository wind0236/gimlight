module Action.PickUpSpec
    ( spec
    ) where

import           Action               (ActionResult (ActionResult, killed, newCellMap, status),
                                       ActionStatus (Failed, Ok))
import           Action.PickUp        (pickUpAction)
import           Actor                (inventoryItems)
import           Control.Lens         ((%~), (&))
import           Control.Monad.State  (StateT (runStateT), execStateT)
import           Control.Monad.Writer (writer)
import           Data.Either          (fromRight)
import           Data.Maybe           (fromJust)
import           Dungeon.Map.Cell     (locateActorAt, removeActorAt,
                                       removeItemAt)
import           Inventory            (addItem)
import           Item                 (getName, herb)
import qualified Localization.Texts   as T
import           SetUp                (initCellMap, initTileCollection,
                                       orcWithFullItemsPosition,
                                       orcWithoutItemsPosition, playerPosition)
import           Test.Hspec           (Spec, it, shouldBe)

spec :: Spec
spec = do
    testPickUpSuccess
    testPickUpVoid
    testPickUpWhenInventoryIsFull

testPickUpSuccess :: Spec
testPickUpSuccess =
    it "returns a Ok result if there is an item at the actor's foot, and player's inventory is not full." $
    result `shouldBe` expected
  where
    result = pickUpAction playerPosition initTileCollection initCellMap
    expected = writer (expectedResult, expectedLog)
    expectedResult =
        ActionResult
            {status = Ok, newCellMap = cellMapAfterPickingUp, killed = []}
    cellMapAfterPickingUp =
        fromRight (error "Failed to pick up.") $
        flip execStateT initCellMap $ do
            _ <- removeItemAt playerPosition
            _ <- removeActorAt playerPosition
            locateActorAt initTileCollection actorWithItem playerPosition
    expectedLog = [T.youGotItem $ getName herb]
    actorWithItem =
        (\(x, _) -> x & inventoryItems %~ (fromJust . addItem herb))
            (fromRight
                 (error "Failed to add an item.")
                 (flip runStateT initCellMap $ removeActorAt playerPosition))

testPickUpVoid :: Spec
testPickUpVoid =
    it "returns a Failed result if there is no item at the actor's foot." $
    result `shouldBe` expected
  where
    result = pickUpAction orcWithoutItemsPosition initTileCollection initCellMap
    expected = writer (expectedResult, expectedLog)
    expectedResult =
        ActionResult {status = Failed, newCellMap = initCellMap, killed = []}
    expectedLog = [T.youGotNothing]

testPickUpWhenInventoryIsFull :: Spec
testPickUpWhenInventoryIsFull =
    it "returns a Failed result if the actor's inventory is full." $
    result `shouldBe` expected
  where
    result =
        pickUpAction orcWithFullItemsPosition initTileCollection initCellMap
    expected = writer (expectedResult, expectedLog)
    expectedResult =
        ActionResult {status = Failed, newCellMap = initCellMap, killed = []}
    expectedLog = [T.bagIsFull]
