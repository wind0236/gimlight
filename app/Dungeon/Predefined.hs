module Dungeon.Predefined
    ( firstEventMap
    ) where

import           Data.Array              ((//))
import           Dungeon.Entity          (Entity)
import           Dungeon.Entity.Friendly (electria)
import           Dungeon.Map.Tile        (TileMap, allWallTiles, floorTile,
                                          wallTile)
import           Dungeon.Types           (Dungeon, dungeon)
import           Linear.V2               (V2 (V2))

firstEventMap :: Entity -> Dungeon
firstEventMap player = dungeon (stringArrayToMap
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
    , "####################"
    ])
    [ player
    , electria $ V2 4 5
    ]

stringArrayToMap :: [String] -> TileMap
stringArrayToMap list = allWallTiles // [((x, y), tile c) | (y, row) <- zip [0..] list, (x, c) <- zip [0..] row]
    where tile c
            | c == '#' = wallTile
            | c == '.' = floorTile
            | otherwise = error "Invalid tile type."
