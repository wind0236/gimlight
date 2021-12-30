module Dungeon.Map.Tile.JSONReaderSpec
    ( spec
    ) where

import           Data.Map                    (empty, union)
import           Dungeon.Map.Tile.JSONReader (addTileFile)
import           SetUp.TileFile              (singleTileFile,
                                              tilesInSingleTileFile,
                                              tilesInUnitedTileFile,
                                              tilesInUnwalkableTileFile,
                                              unitedTileFile,
                                              unwalkableTileFile)
import           Test.Hspec                  (Spec, describe, it, runIO,
                                              shouldBe)

spec :: Spec
spec = do
    testAddTileFile
    testAddUnwalkableTileFile

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
