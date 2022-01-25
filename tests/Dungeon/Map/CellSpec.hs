module Dungeon.Map.CellSpec
    ( spec
    ) where

import           Control.Lens     ((^.))
import           Dungeon.Map.Cell (allWallTiles, tileIdentifierLayerAt, upper)
import           Dungeon.Map.Tile (wallTile)
import           Linear.V2        (V2 (V2))
import           Test.Hspec       (Spec, describe, it)
import           Test.QuickCheck  (property)

spec :: Spec
spec = testAllWallTiles

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
            ((== Just wallTile) . (^. upper))
            (tileIdentifierLayerAt c cellMap)
