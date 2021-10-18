module Dungeon.Predefined.BatsCave
    ( batsDungeon
    ) where

import           Dungeon          (Dungeon, DungeonKind (DungeonType), dungeon)
import           Dungeon.Generate (generateDungeon)
import           Linear.V2        (V2 (V2))
import           System.Random    (StdGen)

batsDungeon :: StdGen -> Dungeon
batsDungeon g = dungeon tileMap monsters items (Just (V2 9 6)) DungeonType
    where (tileMap, monsters, items, _, _) = generateDungeon g 10 5 8 (V2 50 50)
