module Dungeon.Init
    ( initDungeon
    ) where
import           Control.Monad.Trans.State (execState)
import           Dungeon                   (Dungeon, updateMap)
import           Dungeon.Actor             (player)
import           Dungeon.Predefined.Beaeve (beaeve)
import           Linear.V2                 (V2 (V2))

initDungeon :: Dungeon
initDungeon =
        let p = player $ V2 5 5
            d = beaeve p
        in execState updateMap d
