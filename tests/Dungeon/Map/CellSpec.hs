module Dungeon.Map.CellSpec
    ( spec
    ) where

import           Control.Lens     (ix, (^.), (^?))
import           Data.Array       (array)
import           Data.Maybe       (isNothing)
import           Dungeon.Map.Cell (allWallTiles, cellAt, tileIdLayer, upper)
import           Dungeon.Map.Tile (wallTile)
import           Linear.V2        (V2 (V2))
import           Test.Hspec       (Spec, describe, it)
import           Test.QuickCheck  (property)

spec :: Spec
spec = do
    testAllWallTiles
    testArrayAccessingOutOfBounds

testAllWallTiles :: Spec
testAllWallTiles =
    describe "allWallTiles" $
    it "returns a cell map filled with walls." $ property propertyFunc
  where
    propertyFunc (width, height) =
        checkFunc (width, height) (allWallTiles (V2 width height))
    checkFunc (width, height) cellMap =
        all
            (\(x, y) -> isWall (V2 x y) cellMap)
            [(x, y) | x <- [0 .. width - 1], y <- [0 .. height - 1]]
    isWall c cellMap =
        maybe
            False
            ((== Just wallTile) . (^. upper) . (^. tileIdLayer))
            (cellAt c cellMap)

testArrayAccessingOutOfBounds :: Spec
testArrayAccessingOutOfBounds =
    describe "Data.Array" $
    it "returns a `Nothing` if we try to access to an index which does not exist with `^?`" $
    isNothing $ arr ^? ix (u + 1)
  where
    arr = array (0, u) [(x, 0 :: Int) | x <- [0 :: Int .. u]]
    u = 3
