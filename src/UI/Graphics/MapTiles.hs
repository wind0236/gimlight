module UI.Graphics.MapTiles
    ( MapTiles
    , mapTiles
    ) where

import           Codec.Picture       (Image (imageHeight, imageWidth),
                                      PixelRGBA8, convertRGBA8, readImage)
import           Codec.Picture.Extra (crop)
import           Data.Array          (Array, listArray)
import           UI.Draw.Config      (tileHeight, tileWidth)

type MapTiles = Array Int (Image PixelRGBA8)

mapTiles :: FilePath -> IO (Maybe MapTiles)
mapTiles path = fmap (cutTileMapToArray . cutTileMap) <$> readTileMapFile path
  where
    cutTileMapToArray tiles = listArray (0, len - 1) tiles
      where
        len = length tiles

readTileMapFile :: FilePath -> IO (Maybe (Image PixelRGBA8))
readTileMapFile path = do
    tileFile <- readImage path
    case tileFile of
        Right x -> return $ convertAndCheck x
        Left _  -> error "Failed to read a tile image file."
  where
    convertAndCheck img =
        if isValidTileMapFile rgbaImage
            then Just rgbaImage
            else Nothing
      where
        rgbaImage = convertRGBA8 img

isValidTileMapFile :: Image PixelRGBA8 -> Bool
isValidTileMapFile img =
    imageWidth img `mod` tileWidth == 0 && imageHeight img `mod` tileHeight == 0

cutTileMap :: Image PixelRGBA8 -> [Image PixelRGBA8]
cutTileMap img =
    [ crop (col * tileWidth) (row * tileHeight) tileWidth tileHeight img
    | row <- [0 .. rowsOfTilesInImage - 1]
    , col <- [0 .. columnsOfTilesInImage - 1]
    ]
  where
    columnsOfTilesInImage = imageWidth img `div` tileWidth
    rowsOfTilesInImage = imageHeight img `div` tileHeight
