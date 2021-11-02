module Dungeon.Init
    ( initDungeon
    ) where

import           Dungeon                   (Dungeon, updateMap)
import           Dungeon.Actor             (player)
import           Dungeon.Predefined.Beaeve (beaeve)
import           Linear.V2                 (V2 (V2))

initDungeon :: Dungeon
initDungeon = case updateMap $ beaeve $ player $ V2 5 5 of
                  Just x  -> x
                  Nothing -> error "Failed to initialize the first map."
