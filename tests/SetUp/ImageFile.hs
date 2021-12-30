module SetUp.ImageFile
    ( singleTileImage
    , singleTileImagePath
    ) where

import           Codec.Picture (Image, PixelRGBA8, convertRGBA8, readImage)

singleTileImage :: Int -> IO (Image PixelRGBA8)
singleTileImage =
    fmap (convertRGBA8 . rightOrError) . readImage . singleTileImagePath
  where
    rightOrError (Right x) = x
    rightOrError (Left x)  = error $ "Failed to load an image: " ++ x

singleTileImagePath :: Int -> FilePath
singleTileImagePath n
    | n < numOfSeparatedTileImages = indexToPath
    | otherwise = "No such file: " ++ indexToPath
  where
    indexToPath = "tests/images/tiles/single_" ++ show n ++ ".png"

numOfSeparatedTileImages :: Int
numOfSeparatedTileImages = 6
