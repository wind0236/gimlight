module Dungeon.Predefined.BatsCave
    ( batsDungeon
    ) where

import           Control.Monad.State     (State, StateT)
import           Coord                   (Coord)
import           Data.Tree               (Tree)
import           Dungeon                 (Dungeon)
import           Dungeon.Generate        (generateMultipleFloorsDungeon)
import           Dungeon.Generate.Config (config)
import           Dungeon.Identifier      (Identifier (BatsCave))
import           Dungeon.Map.Tile        (TileCollection)
import           IndexGenerator          (IndexGenerator)
import           Linear.V2               (V2 (V2))
import           System.Random           (StdGen)

batsDungeon ::
       TileCollection
    -> StateT IndexGenerator (State StdGen) (Tree Dungeon, Coord)
batsDungeon ts = generateMultipleFloorsDungeon ts cfg BatsCave
  where
    cfg = config 3 10 5 8 (V2 50 50)
