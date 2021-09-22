module Dungeon.GameMap
    ( GameMap
    , allWallTiles
    ) where

import           Data.Array      (Array)
import           Data.Array.Base (array)
import           Dungeon.Size    (height, width)
import           Dungeon.Tile    (Tile, wallTile)

type GameMap = Array (Int, Int) Tile

allWallTiles :: GameMap
allWallTiles = array ((0, 0), (width - 1, height - 1))
    [((x, y), wallTile) | x <- [0 .. width - 1], y <- [0 .. height - 1]]
