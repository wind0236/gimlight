module Dungeon.Predefined.BatsCave
    ( batsDungeon
    ) where

import           Coord            (Coord)
import           Dungeon          (Dungeon)
import           Dungeon.Generate (generateDungeon)
import           Linear.V2        (V2 (V2))
import           System.Random    (StdGen)

batsDungeon :: StdGen -> (Coord, Dungeon)
batsDungeon g = (pos, d)
    where (d, pos, _) = generateDungeon g 10 5 8 (V2 50 50)
