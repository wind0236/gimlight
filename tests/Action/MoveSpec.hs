module Action.MoveSpec
    ( spec
    ) where

import           Action               (ActionResult (ActionResult, killed, newDungeon, status),
                                       ActionResultWithLog,
                                       ActionStatus (Failed, Ok))
import           Action.Move          (moveAction)
import           Actor                (Actor, player)
import           Actor.Monsters       (orc)
import           Control.Monad.Writer (writer)
import           Coord                (Coord)
import           Data.Array           (array)
import           Data.Maybe           (fromJust)
import           Dungeon              (Dungeon, dungeon, pushActor)
import           Dungeon.Identifier   (Identifier (Beaeve))
import           Dungeon.Map.Cell     (TileIdLayer (TileIdLayer), cellMap,
                                       locateActorAt)
import           Dungeon.Map.Tile     (TileCollection, tile)
import           IndexGenerator       (generator)
import           Linear.V2            (V2 (V2))
import qualified Localization.Texts   as T
import           Test.Hspec           (Spec, it, shouldBe)

spec :: Spec
spec = do
    testMoveSucceed
    testTriedToMoveToUnwalkablePlace
    testTriedToMoveWhereActorExists

testMoveSucceed :: Spec
testMoveSucceed =
    it "succeeds to move if no actor is on the destination and the destination is walkable" $
    resultWhenMoveOffsetTo moveTo `shouldBe` succeed moveTo
  where
    moveTo = V2 1 0

testTriedToMoveToUnwalkablePlace :: Spec
testTriedToMoveToUnwalkablePlace =
    it "fails to move because the destination is not walkable." $
    resultWhenMoveOffsetTo (V2 0 1) `shouldBe` failed

testTriedToMoveWhereActorExists :: Spec
testTriedToMoveWhereActorExists =
    it "fails to move because there is an actor on the destination." $
    resultWhenMoveOffsetTo (V2 1 1) `shouldBe` failed

succeed :: V2 Int -> ActionResultWithLog
succeed offset = writer (result, [])
  where
    result =
        ActionResult {status = Ok, newDungeon = dungeonWithPlayer, killed = []}
    dungeonWithPlayer = pushActor (playerPosition + offset) p d
    (p, d) = initDungeonAndPlayer

failed :: ActionResultWithLog
failed = writer (result, l)
  where
    result =
        ActionResult
            {status = Failed, newDungeon = dungeonWithPlayer, killed = []}
    l = [T.youCannotMoveThere]
    dungeonWithPlayer = pushActor playerPosition p d
    (p, d) = initDungeonAndPlayer

resultWhenMoveOffsetTo :: V2 Int -> ActionResultWithLog
resultWhenMoveOffsetTo offset =
    moveAction offset playerPosition p initTileCollection d
  where
    (p, d) = initDungeonAndPlayer

initDungeonAndPlayer :: (Actor, Dungeon)
initDungeonAndPlayer = (p, dungeon cm Beaeve)
  where
    cm =
        fromJust $
        locateActorAt (fst $ orc g') (V2 1 1) $
        cellMap $
        array
            (V2 0 0, V2 1 1)
            [ (V2 0 0, walkable)
            , (V2 1 0, walkable)
            , (V2 0 1, unwalkable)
            , (V2 1 1, walkable)
            ]
    (p, g') = player generator
    walkable = TileIdLayer (Just 0) (Just 0)
    unwalkable = TileIdLayer (Just 1) (Just 0)

initTileCollection :: TileCollection
initTileCollection = array (0, 1) [(0, tile True True), (1, tile False True)]

playerPosition :: Coord
playerPosition = V2 0 0
