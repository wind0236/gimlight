module Dungeon.Init
    ( initDungeon
    ) where

import           Actor                     (player)
import           Control.Lens              ((%%~), (&))
import           Data.Maybe                (fromMaybe)
import           Dungeon                   (Dungeon, cellMap, updateMap)
import           Dungeon.Map.Cell          (locateActorAt)
import           Dungeon.Map.Tile          (TileCollection)
import           Dungeon.Predefined.Beaeve (beaeve)
import           IndexGenerator            (IndexGenerator)
import           Linear.V2                 (V2 (V2))

initDungeon :: IndexGenerator -> TileCollection -> IO (Dungeon, IndexGenerator)
initDungeon ig ts = do
    (beaeve', ig'') <- beaeve ig'
    let d =
            fromMaybe (error "Failed to generate an initial dungeon.") $
            (beaeve' & cellMap %%~ locateActorAt player' (V2 5 5)) >>=
            updateMap ts
    return (d, ig'')
  where
    (player', ig') = player ig
