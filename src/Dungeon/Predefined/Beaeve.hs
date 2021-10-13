module Dungeon.Predefined.Beaeve
    ( beaeve
    ) where

import           Data.Array              ((//))
import           Dungeon.Entity          (Entity)
import           Dungeon.Entity.Friendly (electria)
import           Dungeon.Map.Tile        (TileMap, allWallTiles, floorTile,
                                          wallTile)
import           Dungeon.Types           (Dungeon, dungeon)
import           Linear.V2               (V2 (V2))

beaeve :: Entity -> Dungeon
beaeve player = dungeon (stringArrayToMap
    [ "####################"
    , "#..................#"
    , "#..................#"
    , "#..................#"
    , "#..................#"
    , "#..................#"
    , "#..................#"
    , "#..................#"
    , "#..................#"
    , "#..................#"
    , "#..................#"
    , "#..................#"
    , "#..................#"
    , "#####...############"
    ])
    [ player
    , electria $ V2 4 5
    ]
    (Just (V2 3 16))
    False

stringArrayToMap :: [String] -> TileMap
stringArrayToMap list = allWallTiles (V2 width height) // [(V2 x y, tile c) | (y, row) <- zip [0..] list, (x, c) <- zip [0..] row]
    where tile c
            | c == '#' = wallTile
            | c == '.' = floorTile
            | otherwise = error "Invalid tile type."
          height = length list
          width = length $ head list
