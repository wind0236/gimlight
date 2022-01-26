module Gimlight.Dungeon.Predefined.GlobalMap
    ( globalMap
    ) where

import           Control.Arrow                   (Arrow (first))
import           Gimlight.Dungeon                (Dungeon, dungeon)
import           Gimlight.Dungeon.Identifier     (Identifier (GlobalMap))
import           Gimlight.Dungeon.Map.JSONReader (readMapTileImage)
import           Gimlight.Dungeon.Map.Tile       (TileCollection)

globalMap :: TileCollection -> IO (Dungeon, TileCollection)
globalMap tc =
    first (`dungeon` GlobalMap) <$> readMapTileImage tc "maps/global_map.json"
