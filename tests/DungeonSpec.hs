module DungeonSpec
    ( spec
    ) where

import           Actor               (player)
import           Control.Lens        ((%~), (&), (^.))
import           Control.Monad.State (execStateT)
import           Data.Array          (array)
import           Data.Either         (fromRight)
import           Dungeon             (dungeon)
import qualified Dungeon             as D
import           Dungeon.Identifier  (Identifier (Beaeve))
import           Dungeon.Map.Cell    (TileIdLayer (TileIdLayer), cellMap,
                                      locateActorAt, positionsAndActors)
import           IndexGenerator      (generator)
import           Linear.V2           (V2 (V2))
import           SetUp               (initTileCollection)
import           Test.Hspec          (Spec, describe, it, shouldBe)

spec :: Spec
spec = testPushActor

testPushActor :: Spec
testPushActor =
    describe "pushActor" $
    it "pushes an actor on an empty coordinate successfully" $
    ((V2 0 0, p) `elem` positionsAndActors (afterPushing ^. D.cellMap)) `shouldBe`
    True
  where
    afterPushing =
        d & D.cellMap %~ fromRight (error "Failed to push an actor.") .
        execStateT (locateActorAt initTileCollection p (V2 0 0))
    d = dungeon cm Beaeve
    (p, _) = player ig
    ig = generator
    cm =
        cellMap $ array (V2 0 0, V2 0 0) [(V2 0 0, TileIdLayer Nothing Nothing)]
