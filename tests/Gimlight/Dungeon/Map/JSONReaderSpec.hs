module Gimlight.Dungeon.Map.JSONReaderSpec
    ( spec
    ) where

import           Data.Map                        (empty, union)
import           Gimlight.Dungeon.Map.Cell       (CellMap)
import           Gimlight.Dungeon.Map.JSONReader (readMapTileImage)
import           Gimlight.Dungeon.Map.Tile       (TileCollection)
import           Gimlight.SetUp.MapFile          (cellMapContainingMultipleFilesTile,
                                                  cellMapOfSingleTileMap,
                                                  cellMapUsingMultipleTileFilesAndTransformation,
                                                  cellMapUsingRotatedTiles,
                                                  mapUsingMultipleTileFiles,
                                                  mapUsingRotatedTiles,
                                                  mapUsingTilesFromMultipleTileFilesAndTransformation,
                                                  rectangleButNotSquareCellMap,
                                                  rectangleButNotSquareMap,
                                                  singleTileMap)
import           Gimlight.SetUp.TileFile         (haskellTile,
                                                  tilesInSingleTileFile,
                                                  tilesInUnitedTileFile)
import           Test.Hspec                      (Spec, context, describe, it,
                                                  runIO, shouldBe)

spec :: Spec
spec = do
    testSingleTileMap
    testReadRectangleButNotSquareMap
    testReadMapUsingMultipleTileFiles
    testReadMapUsingRotatedTiles
    testReadMapUsingMultipleTileFilesAndTransformation

testSingleTileMap :: Spec
testSingleTileMap =
    context "Single tile map" $
    runIO tilesInSingleTileFile >>=
    testReadMapTileImage singleTileMap cellMapOfSingleTileMap

testReadRectangleButNotSquareMap :: Spec
testReadRectangleButNotSquareMap =
    context "Not square map" $
    runIO tilesInSingleTileFile >>=
    testReadMapTileImage rectangleButNotSquareMap rectangleButNotSquareCellMap

testReadMapUsingMultipleTileFiles :: Spec
testReadMapUsingMultipleTileFiles =
    context "Map using multiple tile files." $
    runIO (union <$> tilesInSingleTileFile <*> tilesInUnitedTileFile) >>=
    testReadMapTileImage
        mapUsingMultipleTileFiles
        cellMapContainingMultipleFilesTile

testReadMapUsingRotatedTiles :: Spec
testReadMapUsingRotatedTiles =
    context "Map using rotated tiles" $
    runIO haskellTile >>=
    testReadMapTileImage mapUsingRotatedTiles cellMapUsingRotatedTiles

testReadMapUsingMultipleTileFilesAndTransformation :: Spec
testReadMapUsingMultipleTileFilesAndTransformation =
    context "Map using multiple tile files and transformation" $
    runIO (union <$> haskellTile <*> tilesInSingleTileFile) >>=
    testReadMapTileImage
        mapUsingTilesFromMultipleTileFilesAndTransformation
        cellMapUsingMultipleTileFilesAndTransformation

testReadMapTileImage :: FilePath -> CellMap -> TileCollection -> Spec
testReadMapTileImage path cm tc = do
    (resultCellMap, resultTile) <- runIO $ readMapTileImage empty path
    describe "readMapTileImage" $ do
        it "loads the map file" $ resultCellMap `shouldBe` cm
        it "loads the tile file" $ resultTile `shouldBe` tc
