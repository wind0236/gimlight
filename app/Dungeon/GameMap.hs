module Dungeon.GameMap
    ( GameMap
    ) where

import           Data.Array   (Array)
import           Dungeon.Tile (Tile)

type GameMap = Array (Int, Int) Tile
