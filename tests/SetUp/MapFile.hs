module SetUp.MapFile
    ( cellMapContainingMultipleFilesTile
    , cellMapOfSingleTileMap
    , rectangleButNotSquareCellMap
    , mapUsingMultipleTileFiles
    , singleTileMap
    , rectangleButNotSquareMap
    ) where

import           Data.Array       (array)
import           Dungeon.Map.Cell (CellMap,
                                   TileIdentifierLayer (TileIdentifierLayer),
                                   cellMap)
import           Linear.V2        (V2 (V2))
import           SetUp.TileFile   (singleTileFile, unitedTileFile)

cellMapContainingMultipleFilesTile :: CellMap
cellMapContainingMultipleFilesTile =
    cellMap $
    array
        (V2 0 0, V2 1 0)
        [ (V2 0 0, TileIdentifierLayer Nothing (Just (singleTileFile, 0)))
        , (V2 1 0, TileIdentifierLayer Nothing (Just (unitedTileFile, 1)))
        ]

cellMapOfSingleTileMap :: CellMap
cellMapOfSingleTileMap =
    cellMap $
    array
        (V2 0 0, V2 0 0)
        [(V2 0 0, TileIdentifierLayer Nothing (Just (singleTileFile, 0)))]

rectangleButNotSquareCellMap :: CellMap
rectangleButNotSquareCellMap =
    cellMap $
    array
        (V2 0 0, V2 1 0)
        [ (V2 x 0, TileIdentifierLayer Nothing (Just (singleTileFile, 0)))
        | x <- [0, 1]
        ]

mapUsingMultipleTileFiles :: FilePath
mapUsingMultipleTileFiles = "tests/maps/multiple_tile_files.json"

singleTileMap :: FilePath
singleTileMap = "tests/maps/single_tile.json"

rectangleButNotSquareMap :: FilePath
rectangleButNotSquareMap = "tests/maps/not_square.json"
