module Dungeon.Predefined.Beaeve
    ( beaeve
    ) where

import           Actor.Friendly.Electria (electria)
import           Data.Maybe              (fromMaybe)
import           Dungeon                 (Dungeon, dungeon)
import           Dungeon.Identifier      (Identifier (Beaeve))
import qualified Dungeon.Map.JSONReader  as JSONReader
import           Dungeon.Map.Tile        (TileMap)
import           IndexGenerator          (IndexGenerator)
import           Linear.V2               (V2 (V2))

beaeve :: IndexGenerator -> IO (Dungeon, IndexGenerator)
beaeve ig = do
    tileMap <- readMapFile
    return (dungeon tileMap [electria'] [] Beaeve, ig')
  where
    (electria', ig') = electria ig (V2 4 5)

readMapFile :: IO TileMap
readMapFile = do
    tileMap <- JSONReader.readMapFile "maps/beaeve.json"
    return $ fromMaybe (error "Failed to read the map file of Beaeve") tileMap
