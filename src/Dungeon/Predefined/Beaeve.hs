module Dungeon.Predefined.Beaeve
    ( beaeve
    ) where

import           Actor.Friendly.Electria (electria)
import           Control.Monad.State     (execStateT)
import           Data.Maybe              (fromMaybe)
import           Dungeon                 (Dungeon, dungeon)
import           Dungeon.Identifier      (Identifier (Beaeve))
import           Dungeon.Map.Cell        (CellMap, locateActorAt)
import qualified Dungeon.Map.JSONReader  as JSONReader
import           Dungeon.Map.Tile        (TileCollection)
import           IndexGenerator          (IndexGenerator)
import           Linear.V2               (V2 (V2))

beaeve :: TileCollection -> IndexGenerator -> IO (Dungeon, IndexGenerator)
beaeve tc ig = do
    tileMap <- readMapFile
    let tileMap' =
            case flip execStateT tileMap $ locateActorAt tc electria' (V2 4 5) of
                Right x -> x
                Left e ->
                    error $ "Failed to generate the Beaeve map: " <> show e
    return (dungeon tileMap' Beaeve, ig')
  where
    (electria', ig') = electria ig

readMapFile :: IO CellMap
readMapFile = do
    tileMap <- JSONReader.readMapFile "maps/beaeve.json"
    return $ fromMaybe (error "Failed to read the map file of Beaeve") tileMap
