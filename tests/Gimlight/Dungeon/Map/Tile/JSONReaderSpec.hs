module Gimlight.Dungeon.Map.Tile.JSONReaderSpec
    ( spec
    ) where

import           Data.Map                             (empty, union)
import           Gimlight.Dungeon.Map.Tile.JSONReader (addTileFile)
import           Gimlight.SetUp.TileFile              (singleTileFile,
                                                       tileWithoutProperties,
                                                       tilesInSingleTileFile,
                                                       tilesInUnitedTileFile,
                                                       tilesInUnwalkableTileFile,
                                                       unitedTileFile,
                                                       unwalkableTileFile)
import           Test.Hspec                           (Spec, describe,
                                                       errorCall, it, runIO,
                                                       shouldBe, shouldThrow)

spec :: Spec
spec = do
    testAddTileFile
    testAddUnwalkableTileFile
    testErrorOnReadingTileWithoutProperties

testAddTileFile :: Spec
testAddTileFile = do
    expected <-
        runIO $ union <$> tilesInSingleTileFile <*> tilesInUnitedTileFile
    result <-
        runIO $ addTileFile unitedTileFile empty >>= addTileFile singleTileFile
    describe "addTileFile" $
        it "loads tile information from files and returns the image paths." $
        result `shouldBe` expected

testAddUnwalkableTileFile :: Spec
testAddUnwalkableTileFile = do
    expected <- runIO tilesInUnwalkableTileFile
    result <- runIO $ addTileFile unwalkableTileFile empty
    describe "addTileFile" $
        it
            "loads tile information from files and returns the image paths. The tile is unwalkable but transparent." $
        result `shouldBe` expected

testErrorOnReadingTileWithoutProperties :: Spec
testErrorOnReadingTileWithoutProperties =
    describe "addTileFile" $
    it "panics if it tries to read a tile that misses necessary proeprties." $
    addTileFile tileWithoutProperties empty `shouldThrow`
    errorCall
        (tileWithoutProperties ++ ": Some tiles miss necessary properties.")
