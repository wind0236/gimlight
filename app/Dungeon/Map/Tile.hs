module Dungeon.Map.Tile
    ( TileMap
    , allWallTiles
    ) where

import           Data.Array      (Array)
import           Data.Array.Base (array)
import qualified Dungeon.Map     as M
import           Dungeon.Size    (height, width)
import           Dungeon.Tile    (Tile, wallTile)

type TileMap = Array (Int, Int) Tile

allWallTiles :: TileMap
allWallTiles = M.generate $ const wallTile
