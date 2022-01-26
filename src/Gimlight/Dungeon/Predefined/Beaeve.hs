module Gimlight.Dungeon.Predefined.Beaeve
    ( beaeve
    ) where

import           Control.Monad.Morph              (MFunctor (hoist), generalize)
import           Control.Monad.State              (MonadTrans (lift), StateT,
                                                   execStateT)
import           Data.Either.Combinators          (fromRight)
import           Gimlight.Actor.Friendly.Electria (electria)
import           Gimlight.Dungeon                 (Dungeon, dungeon)
import           Gimlight.Dungeon.Identifier      (Identifier (Beaeve))
import           Gimlight.Dungeon.Map.Cell        (locateActorAt)
import           Gimlight.Dungeon.Map.JSONReader  (readMapTileImage)
import           Gimlight.Dungeon.Map.Tile        (TileCollection)
import           Gimlight.IndexGenerator          (IndexGenerator)
import           Linear.V2                        (V2 (V2))

beaeve :: TileCollection -> StateT IndexGenerator IO (Dungeon, TileCollection)
beaeve tc = do
    electria' <- hoist generalize electria
    (cm, tc') <- lift $ readMapTileImage tc "maps/beaeve.json"
    let cm' =
            fromRight (error "Failed to place a NPC.") . flip execStateT cm $
            locateActorAt tc' electria' (V2 4 5)
    return (dungeon cm' Beaeve, tc')
