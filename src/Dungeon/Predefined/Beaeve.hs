module Dungeon.Predefined.Beaeve
    ( beaeve
    ) where

import           Actor.Friendly.Electria (electria)
import           Control.Monad.Morph     (MFunctor (hoist), generalize)
import           Control.Monad.State     (MonadTrans (lift), StateT, execStateT)
import           Data.Either.Combinators (fromRight)
import           Dungeon                 (Dungeon, dungeon)
import           Dungeon.Identifier      (Identifier (Beaeve))
import           Dungeon.Map.Cell        (locateActorAt)
import           Dungeon.Map.JSONReader  (readMapTileImage)
import           Dungeon.Map.Tile        (TileCollection)
import           IndexGenerator          (IndexGenerator)
import           Linear.V2               (V2 (V2))

beaeve :: TileCollection -> StateT IndexGenerator IO (Dungeon, TileCollection)
beaeve tc = do
    electria' <- hoist generalize electria
    (cm, tc') <- lift $ readMapTileImage tc "maps/beaeve.json"
    let cm' =
            fromRight (error "Failed to place a NPC.") . flip execStateT cm $
            locateActorAt tc' electria' (V2 4 5)
    return (dungeon cm' Beaeve, tc')
