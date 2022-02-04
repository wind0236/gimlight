module Gimlight.Dungeon.Map.JSONReaderSpec
  ( spec,
  )
where

import Gimlight.Dungeon.Map.Cell (CellMap)
import Gimlight.Dungeon.Map.JSONReader (readMapFile)
import Gimlight.SetUp.MapFile
  ( cellMapContainingMultipleFilesTile,
    cellMapOfSingleTileMap,
    cellMapUsingMultipleTileFilesAndTransformation,
    cellMapUsingRotatedTiles,
    mapUsingMultipleTileFiles,
    mapUsingRotatedTiles,
    mapUsingTilesFromMultipleTileFilesAndTransformation,
    rectangleButNotSquareCellMap,
    rectangleButNotSquareMap,
    singleTileMap,
  )
import Test.Hspec
  ( Spec,
    context,
    describe,
    it,
    runIO,
    shouldBe,
  )

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
    testReadMapFile singleTileMap cellMapOfSingleTileMap

testReadRectangleButNotSquareMap :: Spec
testReadRectangleButNotSquareMap =
  context "Not square map" $
    testReadMapFile rectangleButNotSquareMap rectangleButNotSquareCellMap

testReadMapUsingMultipleTileFiles :: Spec
testReadMapUsingMultipleTileFiles =
  context "Map using multiple tile files." $
    testReadMapFile mapUsingMultipleTileFiles cellMapContainingMultipleFilesTile

testReadMapUsingRotatedTiles :: Spec
testReadMapUsingRotatedTiles =
  context "Map using rotated tiles" $
    testReadMapFile mapUsingRotatedTiles cellMapUsingRotatedTiles

testReadMapUsingMultipleTileFilesAndTransformation :: Spec
testReadMapUsingMultipleTileFilesAndTransformation =
  context "Map using multiple tile files and transformation" $
    testReadMapFile
      mapUsingTilesFromMultipleTileFilesAndTransformation
      cellMapUsingMultipleTileFilesAndTransformation

testReadMapFile :: FilePath -> CellMap -> Spec
testReadMapFile path cm = do
  resultCellMap <- runIO $ readMapFile path
  describe "readMapTileImage" $
    it "loads the map file" $ resultCellMap `shouldBe` cm
