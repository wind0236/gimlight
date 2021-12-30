{-# LANGUAGE OverloadedStrings #-}

module Dungeon.Map.Tile.JSONReader
    ( addTileFile
    ) where

import           Codec.Picture       (Image (imageHeight, imageWidth),
                                      PixelRGBA8, convertRGBA8, readImage)
import           Codec.Picture.Extra (crop)
import           Control.Applicative (ZipList (ZipList, getZipList))
import           Control.Lens        (filtered, has, only, (^..), (^?))
import           Control.Monad       (guard, (>=>))
import           Data.Aeson.Lens     (_Bool, _Integer, _String, key, values)
import           Data.Either         (fromRight)
import           Data.Map            (insert)
import           Data.Maybe          (fromMaybe)
import           Data.Text           (Text, unpack)
import           Dungeon.Map.Tile    (Tile, TileCollection, tile)
import           System.Directory    (canonicalizePath,
                                      makeRelativeToCurrentDirectory)
import           System.FilePath     (dropFileName, (</>))
import           UI.Draw.Config      (tileHeight, tileWidth)

addTileFile :: FilePath -> TileCollection -> IO TileCollection
addTileFile path tc = do
    canonicalizedPathToJson <- canonicalizeAsRelative path
    foldl (\acc (idx, t) -> insert (canonicalizedPathToJson, idx) t acc) tc <$>
        indexAndTile path
  where
    canonicalizeAsRelative = canonicalizePath >=> makeRelativeToCurrentDirectory

getImagePath :: String -> Text
getImagePath json =
    fromMaybe
        (error "A tile file must associate with an image file.")
        (json ^? key "image" . _String)

indexAndTile :: FilePath -> IO [(Int, Tile)]
indexAndTile path = do
    json <- readFile path
    let imagePath = dropFileName path </> unpack (getImagePath json)
    fmap
        (zip (getIds json) . getZipList .
         (tile <$> walkables json <*> transparents json <*>))
        (images imagePath)
  where
    images = fmap ZipList . readAndCutTileImageFile
    transparents = ZipList . getTransparent
    walkables = ZipList . getWalkable

getIds :: String -> [Int]
getIds json =
    fromInteger <$> json ^.. (key "tiles" . values . key "id") . _Integer

getTransparent :: String -> [Bool]
getTransparent = getBoolProperty "transparent"

getWalkable :: String -> [Bool]
getWalkable = getBoolProperty "walkable"

getBoolProperty :: Text -> String -> [Bool]
getBoolProperty property json =
    json ^.. key "tiles" . values . key "properties" . values .
    filtered (has (key "name" . _String . only property)) .
    key "value" .
    _Bool

readAndCutTileImageFile :: FilePath -> IO [Image PixelRGBA8]
readAndCutTileImageFile = fmap cutTileMap . readTileImageFile

readTileImageFile :: FilePath -> IO (Image PixelRGBA8)
readTileImageFile path = do
    tileFile <- convertRGBA8 . fromRight (error noSuchImage) <$> readImage path
    guard $ isValidTileMapFile tileFile
    return tileFile
  where
    noSuchImage = path ++ " not found."

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
