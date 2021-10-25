module Dungeon.Predefined.BatsCave
    ( batsDungeon
    ) where

import           Coord            (Coord)
import           Dungeon          (Dungeon, DungeonKind (DungeonType), dungeon)
import           Dungeon.Generate (generateDungeon)
import           Linear.V2        (V2 (V2))
import           System.Random    (StdGen)

batsDungeon :: StdGen -> (Coord, Dungeon)
batsDungeon g = (pos, dungeon tileMap monsters items DungeonType)
    where (tileMap, monsters, items, pos, _) = generateDungeon g 10 5 8 (V2 50 50)
