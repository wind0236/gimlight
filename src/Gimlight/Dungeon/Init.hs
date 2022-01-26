module Gimlight.Dungeon.Init
    ( initDungeon
    ) where

import           Control.Lens                       ((%~), (&))
import           Control.Monad.Morph                (MFunctor (hoist),
                                                     generalize)
import           Control.Monad.State                (StateT, execStateT)
import           Data.Either.Combinators            (fromRight)
import           Data.Maybe                         (fromMaybe)
import           Gimlight.Actor                     (player)
import           Gimlight.Dungeon                   (Dungeon, cellMap)
import           Gimlight.Dungeon.Map.Cell          (locateActorAt,
                                                     updateExploredMap,
                                                     updatePlayerFov)
import           Gimlight.Dungeon.Map.Tile          (TileCollection)
import           Gimlight.Dungeon.Predefined.Beaeve (beaeve)
import           Gimlight.IndexGenerator            (IndexGenerator)
import           Linear.V2                          (V2 (V2))

initDungeon ::
       TileCollection -> StateT IndexGenerator IO (Dungeon, TileCollection)
initDungeon tc = do
    (beaeve', tc') <- beaeve tc
    player' <- hoist generalize player
    let d = beaeve' & cellMap %~ initBeaeve tc' player'
    return (d, tc')
  where
    initBeaeve tc' p cm' =
        updateExploredMap .
        fromMaybe (error "Failed to update the player FoV.") .
        updatePlayerFov tc' . fromRight (error "Failed to locate the player.") $
        execStateT (locateActorAt tc' p (V2 10 10)) cm'
