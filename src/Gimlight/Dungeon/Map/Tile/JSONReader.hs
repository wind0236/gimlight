{-# LANGUAGE OverloadedStrings #-}

module Gimlight.Dungeon.Map.Tile.JSONReader
    ( readTileFileRecursive
    ) where

import           Codec.Picture              (Image (imageData, imageHeight, imageWidth),
                                             PixelRGBA8, convertRGBA8,
                                             readImage)
import           Codec.Picture.Extra        (crop, flipHorizontally,
                                             flipVertically)
import           Control.Applicative        (ZipList (ZipList, getZipList))
import           Control.Lens               (filtered, has, only, (&), (^..),
                                             (^?))
import           Control.Monad              (guard, unless, (>=>))
import           Data.Aeson.Lens            (_Bool, _Integer, _String, key,
                                             values)
import           Data.Bits                  (Bits (bit), (.|.))
import           Data.Either                (fromRight)
import           Data.Foldable              (foldlM)
import           Data.List                  (transpose)
import           Data.List.Split            (chunksOf)
import           Data.Map                   (empty, insert)
import           Data.Text                  (Text, unpack)
import qualified Data.Vector.Storable       as V
import           Gimlight.Data.Maybe        (expectJust)
import           Gimlight.Dungeon.Map.Tile  (Tile, TileCollection, getImage,
                                             isTransparent, isWalkable, tile)
import           Gimlight.UI.Draw.Config    (tileHeight, tileWidth)
import           System.Directory           (canonicalizePath,
                                             makeRelativeToCurrentDirectory)
import           System.Directory.Recursive (getFilesRecursive)
import           System.FilePath            (dropFileName, (</>))

readTileFileRecursive :: FilePath -> IO TileCollection
readTileFileRecursive dir =
    getFilesRecursive dir >>= foldlM (flip addTileFile) empty

addTileFile :: FilePath -> TileCollection -> IO TileCollection
addTileFile path tc = do
    canonicalizedPathToJson <- canonicalizeAsRelative path
    fmap
        (foldl (\acc (idx, t) -> insert (canonicalizedPathToJson, idx) t acc) tc .
         generateTransformedTiles)
        (indexAndTile path)
  where
    canonicalizeAsRelative = canonicalizePath >=> makeRelativeToCurrentDirectory

getImagePath :: String -> Text
getImagePath json =
    expectJust
        "A tile file must associate with an image file."
        (json ^? key "image" . _String)

generateTransformedTiles :: [(Int, Tile)] -> [(Int, Tile)]
generateTransformedTiles = concatMap $ uncurry generatePossibleTransformations
  where
    generatePossibleTransformations idx t =
        [ ( transformationFlagsSetter d v h idx
          , mapTileImage (transformImage d v h) t)
        | (d, v, h) <- diagonalVertialHorizontal
        ]
    mapTileImage f t = tile (isWalkable t) (isTransparent t) (f $ getImage t)
    transformImage d v h =
        applyFunctionIf h flipHorizontally . applyFunctionIf v flipVertically .
        applyFunctionIf d swapImageXY
    swapImageXY :: Image PixelRGBA8 -> Image PixelRGBA8
    swapImageXY img = img {imageData = swapVectorXY $ imageData img}
    swapVectorXY =
        V.fromList . concat . concat . transpose . chunksOf tileWidth .
        chunksOf 4 .
        V.toList
    transformationFlagsSetter :: Bool -> Bool -> Bool -> Int -> Int
    transformationFlagsSetter d v h =
        (setBitIf d 29 .|. setBitIf v 30 .|. setBitIf h 31 .|.)
    setBitIf cond b
        | cond = bit b
        | otherwise = 0
    applyFunctionIf cond f
        | cond = f
        | otherwise = id
    diagonalVertialHorizontal =
        (,,) <$> [False, True] <*> [False, True] <*> [False, True]

indexAndTile :: FilePath -> IO [(Int, Tile)]
indexAndTile path = do
    json <- readFile path
    let imagePath = dropFileName path </> unpack (getImagePath json)
    unless (allTilesHaveNecessaryProperties json) $ error $ path ++
        ": Some tiles miss necessary properties."
    fmap
        (zip (getIds json) . getZipList .
         (tile <$> walkables json <*> transparents json <*>))
        (images imagePath)
  where
    images = fmap ZipList . readAndCutTileImageFile
    transparents = ZipList . getTransparent
    walkables = ZipList . getWalkable

allTilesHaveNecessaryProperties :: String -> Bool
allTilesHaveNecessaryProperties json =
    all ((== tileCount) . length . (json &)) [getTransparent, getWalkable]
  where
    tileCount = getTileCount json

getTileCount :: String -> Int
getTileCount json =
    fromInteger $ expectJust "No tilecount entry." $ json ^? key "tilecount" .
    _Integer

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
