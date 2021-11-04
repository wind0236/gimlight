module Dungeon.Predefined.Beaeve
    ( beaeve
    ) where

import           Data.Array             ((//))
import           Dungeon                (Dungeon, DungeonKind (Town), dungeon)
import           Dungeon.Actor          (Actor)
import           Dungeon.Actor.Friendly (electria)
import           Dungeon.Map.Tile       (TileMap, allWallTiles, floorTile,
                                         wallTile)
import           Linear.V2              (V2 (V2))

beaeve :: Actor -> Dungeon
beaeve player =
    dungeon
        (stringArrayToMap
             [ "#########################"
             , "#.......................#"
             , "#.......................#"
             , "#.......................#"
             , "#.......................#"
             , "#.......................#"
             , "#.......................#"
             , "#.......................#"
             , "#.......................#"
             , "#.......................#"
             , "#.......................#"
             , "#.......................#"
             , "#.......................#"
             , "#####......##############"
             ])
        [player, electria $ V2 4 5]
        []
        Town

stringArrayToMap :: [String] -> TileMap
stringArrayToMap list =
    allWallTiles (V2 width height) //
    [(V2 x y, tile c) | (y, row) <- zip [0 ..] list, (x, c) <- zip [0 ..] row]
  where
    tile c
        | c == '#' = wallTile
        | c == '.' = floorTile
        | otherwise = error "Invalid tile type."
    height = length list
    width = length $ head list
