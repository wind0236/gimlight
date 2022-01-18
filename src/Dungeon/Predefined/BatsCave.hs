module Dungeon.Predefined.BatsCave
    ( batsDungeon
    ) where

import           Control.Monad.State     (State)
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
       IndexGenerator
    -> TileCollection
    -> State StdGen (Tree Dungeon, Coord, IndexGenerator)
batsDungeon ig ts = generateMultipleFloorsDungeon ig ts cfg BatsCave
  where
    cfg = config 3 10 5 8 (V2 50 50)
