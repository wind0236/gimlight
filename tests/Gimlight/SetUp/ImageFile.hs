module Gimlight.SetUp.ImageFile
    ( singleTileImage
    , generateTileImage
    , haskellTileImage
    , singleTileImagePath
    ) where

import           Codec.Picture (Image, PixelRGBA8, convertRGBA8, readImage)

generateTileImage :: Int -> IO (Image PixelRGBA8)
generateTileImage = readImageOrError . generateTileImagePath

singleTileImage :: Int -> IO (Image PixelRGBA8)
singleTileImage = readImageOrError . singleTileImagePath

haskellTileImage :: IO (Image PixelRGBA8)
haskellTileImage = readImageOrError haskellImagePath

singleTileImagePath :: Int -> FilePath
singleTileImagePath n
    | n < numOfSeparatedTileImages = indexToPath
    | otherwise = "No such file: " ++ indexToPath
  where
    indexToPath = "tests/images/tiles/single_" ++ show n ++ ".png"

generateTileImagePath :: Int -> FilePath
generateTileImagePath n
    | n < numOfGenerateTileImages = indexToPath
    | otherwise = "No such file: " ++ indexToPath
  where
    indexToPath = "tests/images/tiles/generate/generate_" ++ show n ++ ".png"

haskellImagePath :: FilePath
haskellImagePath = "tests/images/tiles/haskell.png"

readImageOrError :: FilePath -> IO (Image PixelRGBA8)
readImageOrError = fmap (convertRGBA8 . rightOrError) . readImage
  where
    rightOrError (Right x) = x
    rightOrError (Left x)  = error $ "Failed to load an image: " ++ x

numOfSeparatedTileImages :: Int
numOfSeparatedTileImages = 6

numOfGenerateTileImages :: Int
numOfGenerateTileImages = 30
