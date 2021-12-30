module Dungeon.Predefined.GlobalMap
    ( globalMap
    ) where

import           Control.Arrow          (Arrow (first))
import           Dungeon                (Dungeon, dungeon)
import           Dungeon.Identifier     (Identifier (GlobalMap))
import           Dungeon.Map.JSONReader (readMapTileImage)
import           Dungeon.Map.Tile       (TileCollection)

globalMap :: TileCollection -> IO (Dungeon, TileCollection)
globalMap tc =
    first (`dungeon` GlobalMap) <$> readMapTileImage tc "maps/global_map.json"
