module Gimlight.Dungeon.Predefined.GlobalMap
    ( globalMap
    ) where

import           Gimlight.Dungeon                (Dungeon, dungeon)
import           Gimlight.Dungeon.Identifier     (Identifier (GlobalMap))
import           Gimlight.Dungeon.Map.JSONReader (readMapFile)

globalMap :: IO Dungeon
globalMap = (`dungeon` GlobalMap) <$> readMapFile "maps/global_map.json"
