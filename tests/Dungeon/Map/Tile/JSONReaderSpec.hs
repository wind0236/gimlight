module Dungeon.Map.Tile.JSONReaderSpec
    ( spec
    ) where

import           Data.Maybe                  (fromJust)
import           Dungeon.Map.Tile.JSONReader (readTileFile)
import           System.FilePath             (equalFilePath)
import           Test.Hspec                  (Spec, describe, it, runIO,
                                              shouldSatisfy)

spec :: Spec
spec = testReadTileFileReturnsImagePath

testReadTileFileReturnsImagePath :: Spec
testReadTileFileReturnsImagePath = do
    result <- runIO $ readTileFile "maps/tiles.json"
    describe "readTileFile" $
        it "returns the path to the corresponding image file." $
        snd (fromJust result) `shouldSatisfy` (`equalFilePath` expected)
  where
    expected = "maps/../images/map_tiles.png"
