module Dungeon.Predefined.Beaeve
    ( beaeve
    ) where

import           Actor.Friendly.Electria (electria)
import           Data.Maybe              (fromMaybe)
import           Dungeon                 (Dungeon, dungeon)
import           Dungeon.Identifier      (Identifier (Beaeve))
import           Dungeon.Map.Cell        (CellMap, locateActorAt)
import qualified Dungeon.Map.JSONReader  as JSONReader
import           IndexGenerator          (IndexGenerator)
import           Linear.V2               (V2 (V2))

beaeve :: IndexGenerator -> IO (Dungeon, IndexGenerator)
beaeve ig = do
    tileMap <- readMapFile
    let tileMap' =
            fromMaybe
                (error "Failed to locate an actor.")
                (locateActorAt electria' (V2 4 5) tileMap)
    return (dungeon tileMap' Beaeve, ig')
  where
    (electria', ig') = electria ig

readMapFile :: IO CellMap
readMapFile = do
    tileMap <- JSONReader.readMapFile "maps/beaeve.json"
    return $ fromMaybe (error "Failed to read the map file of Beaeve") tileMap
