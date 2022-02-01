module Gimlight.Dungeon.Map.Tile.JSONReaderSpec
    ( spec
    ) where

import           Data.Map                             (unions)
import           Gimlight.Dungeon.Map.Tile.JSONReader (readTileFileRecursive)
import           Gimlight.SetUp.TileFile              (haskellTile,
                                                       tileWithoutProperties,
                                                       tilesInSingleTileFile,
                                                       tilesInUnitedTileFile,
                                                       tilesInUnwalkableTileFile)
import           Test.Hspec                           (Spec, describe,
                                                       errorCall, it, runIO,
                                                       shouldBe, shouldThrow)

spec :: Spec
spec = do
    testReadTileFilesRecursive
    testErrorOnReadingTileWithoutProperties

testReadTileFilesRecursive :: Spec
testReadTileFilesRecursive = do
    expected <- runIO $ unions <$> sequence tiles
    result <- runIO $ readTileFileRecursive "tests/tiles/valid/"
    describe "readTileFilesRecursive" $
        it "reads all tile files in a directory recursively." $
        result `shouldBe` expected
  where
    tiles =
        [ tilesInUnitedTileFile
        , tilesInSingleTileFile
        , tilesInUnwalkableTileFile
        , haskellTile
        ]

testErrorOnReadingTileWithoutProperties :: Spec
testErrorOnReadingTileWithoutProperties =
    describe "addTileFile" $
    it "panics if it tries to read a tile that misses necessary proeprties." $
    readTileFileRecursive "tests/tiles/invalid/" `shouldThrow`
    errorCall
        (tileWithoutProperties ++ ": Some tiles miss necessary properties.")
