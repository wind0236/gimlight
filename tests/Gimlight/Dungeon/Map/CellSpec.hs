module Gimlight.Dungeon.Map.CellSpec
    ( spec
    ) where

import           Control.Lens              ((^.))
import           Gimlight.Dungeon.Map.Cell (allWallTiles, tileIdLayerAt,
                                            upper)
import           Gimlight.Dungeon.Map.Tile (wallTile)
import           Linear.V2                 (V2 (V2))
import           Test.Hspec                (Spec, describe, it)
import           Test.QuickCheck           (property)

spec :: Spec
spec = testAllWallTiles

testAllWallTiles :: Spec
testAllWallTiles =
    describe "allWallTiles" $
    it "returns a cell map filled with walls." $ property propertyFunc
  where
    propertyFunc (width, height) =
        checkFunc (width, height) (allWallTiles (V2 width height))
    checkFunc (width, height) cm =
        all
            (\(x, y) -> isWall (V2 x y) cm)
            [(x, y) | x <- [0 .. width - 1], y <- [0 .. height - 1]]
    isWall c cm =
        maybe
            False
            ((== Just wallTile) . (^. upper))
            (tileIdLayerAt c cm)
