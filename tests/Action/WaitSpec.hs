module Action.WaitSpec
    ( spec
    ) where

import           Action               (ActionResult (ActionResult, killed, newDungeon, status),
                                       ActionStatus (Ok))
import           Action.Wait          (waitAction)
import           Actor                (player)
import           Control.Lens         ((%~), (&))
import           Control.Monad.Writer (writer)
import           Data.Array           (array)
import           Data.Maybe           (fromJust)
import           Dungeon              (dungeon)
import qualified Dungeon              as D
import           Dungeon.Identifier   (Identifier (Beaeve))
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
    result = waitAction playerPosition p tc dungeonWithoutPlayer
    expected = writer (expectedResult, expectedLog)
    expectedResult =
        ActionResult {status = Ok, newDungeon = dungeonWithPlayer, killed = []}
    expectedLog = []
    p = fst $ player generator
    tc = array (0, 0) [(0, tile True True)]
    dungeonWithPlayer =
        dungeonWithoutPlayer &
        D.cellMap %~ (fromJust . locateActorAt p playerPosition)
    dungeonWithoutPlayer = dungeon cm Beaeve
    cm =
        cellMap $ array (V2 0 0, V2 0 0) [(V2 0 0, TileIdLayer Nothing Nothing)]
    playerPosition = V2 0 0
