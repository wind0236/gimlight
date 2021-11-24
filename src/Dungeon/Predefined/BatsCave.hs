module Dungeon.Predefined.BatsCave
    ( batsDungeon
    ) where

import           Coord                   (Coord)
import           Data.Tree               (Tree)
import           Dungeon                 (Dungeon)
import           Dungeon.Generate        (generateMultipleFloorsDungeon)
import           Dungeon.Generate.Config (Config (Config, mapSize, maxRooms, numOfFloors, roomMaxSize, roomMinSize))
import           Dungeon.Identifier      (Identifier (BatsCave))
import           Dungeon.Map.Tile        (TileCollection)
import           Linear.V2               (V2 (V2))
import           System.Random           (StdGen)

batsDungeon :: StdGen -> TileCollection -> (Coord, Tree Dungeon)
batsDungeon g ts = (pos, d)
  where
    (d, pos, _) = generateMultipleFloorsDungeon g ts cfg BatsCave
    cfg =
        Config
            { numOfFloors = 3
            , maxRooms = 10
            , roomMinSize = 5
            , roomMaxSize = 8
            , mapSize = V2 50 50
            }
