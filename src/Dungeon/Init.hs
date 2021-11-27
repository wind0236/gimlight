module Dungeon.Init
    ( initDungeon
    ) where

import           Actor                     (player)
import           Data.Maybe                (fromMaybe)
import           Dungeon                   (Dungeon, pushActor, updateMap)
import           Dungeon.Map.Tile          (TileCollection)
import           Dungeon.Predefined.Beaeve (beaeve)
import           IndexGenerator            (IndexGenerator)
import           Linear.V2                 (V2 (V2))

initDungeon :: IndexGenerator -> TileCollection -> IO (Dungeon, IndexGenerator)
initDungeon ig ts = do
    (beaeve', ig'') <- beaeve ig'
    let d =
            fromMaybe (error "Failed to initialize the first map.") $
            updateMap ts $ pushActor (V2 5 5) player' beaeve'
    return (d, ig'')
  where
    (player', ig') = player ig
