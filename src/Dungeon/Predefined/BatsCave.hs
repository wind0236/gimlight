module Dungeon.Predefined.BatsCave
    ( batsDungeon
    ) where

import           Coord              (Coord)
import           Data.Tree          (Tree)
import           Dungeon            (Dungeon)
import           Dungeon.Generate   (generateMultipleFloorsDungeon)
import           Dungeon.Identifier (Identifier (BatsCave))
import           Dungeon.Map.Tile   (TileCollection)
import           Linear.V2          (V2 (V2))
import           System.Random      (StdGen)

batsDungeon :: StdGen -> TileCollection -> (Coord, Tree Dungeon)
batsDungeon g ts = (pos, d)
  where
    (d, pos, _) =
        generateMultipleFloorsDungeon g ts 3 10 5 8 (V2 50 50) BatsCave
