module Dungeon.Map.Explored
    ( ExploredMap
    , initExploredMap
    , updateExploredMap
    ) where

import           Data.Array       (assocs, (!), (//))
import           Dungeon.Map.Bool (BoolMap, emptyBoolMap)
import           Dungeon.Map.Fov  (Fov)
import           Linear.V2        (V2)

type ExploredMap = BoolMap;

initExploredMap :: V2 Int -> ExploredMap
initExploredMap = emptyBoolMap

updateExploredMap :: ExploredMap -> Fov -> ExploredMap
updateExploredMap e f = e // [(pos, (e ! pos) || value) | (pos, value) <- assocs f]
