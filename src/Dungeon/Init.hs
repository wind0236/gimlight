module Dungeon.Init
    ( initDungeon
    ) where

import           Actor                     (player)
import           Control.Lens              ((%~), (&))
import           Control.Monad.State       (execStateT)
import           Data.Either.Combinators   (fromRight)
import           Data.Maybe                (fromMaybe)
import           Dungeon                   (Dungeon, cellMap)
import           Dungeon.Map.Cell          (locateActorAt, updateExploredMap,
                                            updatePlayerFov)
import           Dungeon.Map.Tile          (TileCollection)
import           Dungeon.Predefined.Beaeve (beaeve)
import           IndexGenerator            (IndexGenerator)
import           Linear.V2                 (V2 (V2))

initDungeon ::
       TileCollection
    -> IndexGenerator
    -> IO (Dungeon, TileCollection, IndexGenerator)
initDungeon tc ig = do
    (beaeve', tc', ig'') <- beaeve tc ig'
    let d = beaeve' & cellMap %~ initBeaeve tc'
    return (d, tc', ig'')
  where
    initBeaeve tc' cm' =
        updateExploredMap .
        fromMaybe (error "Failed to update the player FoV.") .
        updatePlayerFov tc' . fromRight (error "Failed to locate the player.") $
        execStateT (locateActorAt tc' player' (V2 5 5)) cm'
    (player', ig') = player ig
