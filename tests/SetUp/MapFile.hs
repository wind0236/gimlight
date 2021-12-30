module SetUp.MapFile
    ( cellMapOfSingleTileMap
    , singleTileMap
    ) where

import           Data.Array       (array)
import           Dungeon.Map.Cell (CellMap,
                                   TileIdentifierLayer (TileIdentifierLayer),
                                   cellMap)
import           Linear.V2        (V2 (V2))
import           SetUp.TileFile   (singleTileFile)

cellMapOfSingleTileMap :: CellMap
cellMapOfSingleTileMap =
    cellMap $
    array
        (V2 0 0, V2 0 0)
        [(V2 0 0, TileIdentifierLayer Nothing (Just (singleTileFile, 0)))]

singleTileMap :: FilePath
singleTileMap = "tests/maps/single_tile.json"
