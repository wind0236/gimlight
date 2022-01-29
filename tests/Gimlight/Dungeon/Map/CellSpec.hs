{-# LANGUAGE QuasiQuotes #-}

module Gimlight.Dungeon.Map.CellSpec
    ( spec
    ) where

import           Control.Lens              ((^.))
import           Data.Array                (array)
import           Data.String.QQ            (s)
import           Gimlight.Dungeon.Map.Cell (TileIdentifierLayer (TileIdentifierLayer),
                                            allWallTiles, cellMap,
                                            tileIdentifierLayerAt, upper)
import           Gimlight.Dungeon.Map.Tile (wallTile)
import           Gimlight.SetUp.CellMap    (initCellMap)
import           Gimlight.SetUp.MapFile    (cellMapContainingMultipleFilesTile,
                                            rectangleButNotSquareCellMap)
import           Linear.V2                 (V2 (V2))
import           Test.Hspec                (Spec, describe, it, shouldBe)
import           Test.QuickCheck           (property)

spec :: Spec
spec = do
    testAllWallTiles
    testShowCellMap

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
            (tileIdentifierLayerAt c cm)

testShowCellMap :: Spec
testShowCellMap =
    describe "CellMap" $
    it "show prints the detailed cell map information." $
    mapM_ testFunc $ zip cellMaps expectations
  where
    testFunc (m, expected) = show m `shouldBe` expected
    cellMaps =
        [ cellMapContainingMultipleFilesTile
        , initCellMap
        , rectangleButNotSquareCellMap
        , differentCellSize
        ]
    differentCellSize =
        cellMap $
        array
            (V2 0 0, V2 0 0)
            [ ( V2 0 0
              , TileIdentifierLayer
                    (Just ("foo.json", 10))
                    (Just ("foo.json", 0)))
            ]
    expectations =
        [ [s|
Upper layer:
+-----+-----+
|     |     |
+-----+-----+

Lower layer:
+-----+-----+
|(0,0)|(1,1)|
+-----+-----+

Actors:
+-----+-----+
|     |     |
+-----+-----+

Tile files:
0: tests/tiles/single.json
1: tests/tiles/united.json|]
        , [s|
Upper layer:
+------+------+------+
|      |      |      |
+------+------+------+
|(0,1) |      |      |
+------+------+------+
|      |      |      |
+------+------+------+
|      |      |      |
+------+------+------+

Lower layer:
+------+------+------+
|      |      |      |
+------+------+------+
|      |      |      |
+------+------+------+
|      |      |      |
+------+------+------+
|      |      |      |
+------+------+------+

Actors:
+------+------+------+
|Player|Orc   |Orc   |
+------+------+------+
|      |      |Orc   |
+------+------+------+
|      |Orc   |      |
+------+------+------+
|Orc   |Orc   |      |
+------+------+------+

Tile files:
0: dummy.json|]
        , [s|
Upper layer:
+-----+-----+
|     |     |
+-----+-----+

Lower layer:
+-----+-----+
|(0,0)|(0,0)|
+-----+-----+

Actors:
+-----+-----+
|     |     |
+-----+-----+

Tile files:
0: tests/tiles/single.json|]
        , [s|
Upper layer:
+------+
|(0,10)|
+------+

Lower layer:
+------+
|(0,0) |
+------+

Actors:
+------+
|      |
+------+

Tile files:
0: foo.json|]
        ]
