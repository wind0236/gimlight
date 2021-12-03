module DungeonSpec
    ( spec
    ) where

import           Actor              (player)
import           Data.Array         (array)
import           Dungeon            (dungeon, getPositionsAndActors, pushActor)
import           Dungeon.Identifier (Identifier (Beaeve))
import           Dungeon.Map.Cell   (TileIdLayer (TileIdLayer), cellMap)
import           IndexGenerator     (generator)
import           Linear.V2          (V2 (V2))
import           Test.Hspec         (Spec, describe, it, shouldBe)

spec :: Spec
spec = testPushActor

testPushActor :: Spec
testPushActor =
    describe "pushActor" $
    it "pushes an actor on an empty coordinate successfully" $
    ((V2 0 0, p) `elem` getPositionsAndActors afterPushing) `shouldBe` True
  where
    afterPushing = pushActor (V2 0 0) p d
    d = dungeon cm Beaeve
    (p, _) = player ig
    ig = generator
    cm =
        cellMap $ array (V2 0 0, V2 0 0) [(V2 0 0, TileIdLayer Nothing Nothing)]
