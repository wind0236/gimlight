module Dungeon.TileMap
    ( TileMap
    , allWallTiles
    ) where

import           Data.Array      (Array)
import           Data.Array.Base (array)
import           Dungeon.Size    (height, width)
import           Dungeon.Tile    (Tile, wallTile)

type TileMap = Array (Int, Int) Tile

allWallTiles :: TileMap
allWallTiles = array ((0, 0), (width - 1, height - 1))
    [((x, y), wallTile) | x <- [0 .. width - 1], y <- [0 .. height - 1]]
