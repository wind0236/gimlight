module Gimlight.SetUp.MapFile
    ( cellMapContainingMultipleFilesTile
    , cellMapOfSingleTileMap
    , rectangleButNotSquareCellMap
    , cellMapUsingRotatedTiles
    , cellMapUsingMultipleTileFilesAndTransformation
    , mapUsingMultipleTileFiles
    , singleTileMap
    , rectangleButNotSquareMap
    , mapUsingRotatedTiles
    , mapUsingTilesFromMultipleTileFilesAndTransformation
    ) where

import           Data.Array                (array)
import           Data.Bits                 (Bits (bit, (.|.)))
import           Gimlight.Dungeon.Map.Cell (CellMap, TileIdLayer (TileIdLayer),
                                            cellMap)
import           Gimlight.SetUp.TileFile   (haskellTilePath, singleTileFile,
                                            unitedTileFile)
import           Linear.V2                 (V2 (V2))

cellMapContainingMultipleFilesTile :: CellMap
cellMapContainingMultipleFilesTile =
    cellMap $
    array
        (V2 0 0, V2 1 0)
        [ (V2 0 0, TileIdLayer Nothing (Just (singleTileFile, 0)))
        , (V2 1 0, TileIdLayer Nothing (Just (unitedTileFile, 1)))
        ]

cellMapOfSingleTileMap :: CellMap
cellMapOfSingleTileMap =
    cellMap $
    array
        (V2 0 0, V2 0 0)
        [(V2 0 0, TileIdLayer Nothing (Just (singleTileFile, 0)))]

rectangleButNotSquareCellMap :: CellMap
rectangleButNotSquareCellMap =
    cellMap $
    array
        (V2 0 0, V2 1 0)
        [(V2 x 0, TileIdLayer Nothing (Just (singleTileFile, 0))) | x <- [0, 1]]

cellMapUsingRotatedTiles :: CellMap
cellMapUsingRotatedTiles =
    cellMap $
    array (V2 0 0, V2 (mapWidth - 1) 0) $
    zipWith (\x t -> (V2 x 0, t)) [0 .. mapWidth - 1] tiles
  where
    tiles = [layerFromDVH d v h | (d, v, h) <- diagonalVertialHorizontal]
    layerFromDVH d v = TileIdLayer Nothing . Just . identifierFromDVH d v
    identifierFromDVH d v h = (haskellTilePath, idMultiplier d v h 0)
    idMultiplier d v h = (bitIf d 29 .|. bitIf v 30 .|. bitIf h 31 .|.)
    bitIf cond b
        | cond = bit b
        | otherwise = 0
    diagonalVertialHorizontal =
        (,,) <$> [False, True] <*> [False, True] <*> [False, True]
    mapWidth = 8

cellMapUsingMultipleTileFilesAndTransformation :: CellMap
cellMapUsingMultipleTileFilesAndTransformation =
    cellMap $
    array
        (V2 0 0, V2 1 0)
        [ (V2 0 0, TileIdLayer Nothing $ Just (haskellTilePath, bit 31))
        , (V2 1 0, TileIdLayer Nothing $ Just (singleTileFile, bit 31))
        ]

mapUsingMultipleTileFiles :: FilePath
mapUsingMultipleTileFiles = "tests/maps/multiple_tile_files.json"

singleTileMap :: FilePath
singleTileMap = "tests/maps/single_tile.json"

rectangleButNotSquareMap :: FilePath
rectangleButNotSquareMap = "tests/maps/not_square.json"

mapUsingRotatedTiles :: FilePath
mapUsingRotatedTiles = "tests/maps/rotation.json"

mapUsingTilesFromMultipleTileFilesAndTransformation :: FilePath
mapUsingTilesFromMultipleTileFilesAndTransformation =
    "tests/maps/transform_and_rotation.json"
