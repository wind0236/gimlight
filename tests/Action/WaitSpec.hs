module Action.WaitSpec
    ( spec
    ) where

import           Action               (ActionResult (ActionResult, killed, newCellMap, status),
                                       ActionStatus (Ok))
import           Action.Wait          (waitAction)
import           Actor                (player)
import           Control.Monad.Writer (writer)
import           Data.Array           (array)
import           Data.Maybe           (fromJust)
import           Dungeon.Map.Cell     (TileIdLayer (TileIdLayer), cellMap,
                                       locateActorAt)
import           Dungeon.Map.Tile     (tile)
import           IndexGenerator       (generator)
import           Linear.V2            (V2 (V2))
import           Test.Hspec           (Spec, describe, it, shouldBe)

spec :: Spec
spec =
    describe "WaitAction" $
    it "returns a Ok result." $ result `shouldBe` expected
  where
    result = waitAction playerPosition tc cellMapWithPlayer
    expected = writer (expectedResult, expectedLog)
    expectedResult =
        ActionResult {status = Ok, newCellMap = cellMapWithPlayer, killed = []}
    expectedLog = []
    p = fst $ player generator
    tc = array (0, 0) [(0, tile True True)]
    cellMapWithPlayer =
        fromJust $ locateActorAt p playerPosition cellMapWithoutPlayer
    cellMapWithoutPlayer =
        cellMap $ array (V2 0 0, V2 0 0) [(V2 0 0, TileIdLayer Nothing Nothing)]
    playerPosition = V2 0 0
