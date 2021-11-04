module Dungeon.Predefined.BatsCave
    ( batsDungeon
    ) where

import           Coord            (Coord)
import           Data.Tree        (Tree)
import           Dungeon          (Dungeon)
import           Dungeon.Generate (generateMultipleFloorsDungeon)
import           Linear.V2        (V2 (V2))
import           System.Random    (StdGen)

batsDungeon :: StdGen -> (Coord, Tree Dungeon)
batsDungeon g = (pos, d)
  where
    (d, pos, _) = generateMultipleFloorsDungeon g 3 10 5 8 (V2 50 50)
