module Dungeon.Map.Explored
    ( ExploredMap
    , initExploredMap
    , updateExploredMap
    ) where

import           Data.Array       (assocs, (!), (//))
import           Dungeon.Map.Bool (BoolMap, emptyBoolMap)
import           Dungeon.Map.Fov  (Fov)

type ExploredMap = BoolMap;

initExploredMap :: ExploredMap
initExploredMap = emptyBoolMap

updateExploredMap :: ExploredMap -> Fov -> ExploredMap
updateExploredMap e f = e // [(pos, (e ! pos) || value) | (pos, value) <- assocs f]
