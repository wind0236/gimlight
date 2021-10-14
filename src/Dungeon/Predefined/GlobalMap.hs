module Dungeon.Predefined.GlobalMap
    ( globalMap
    ) where
import           Data.Array       ((//))
import           Dungeon.Map.Tile (TileMap, allWallTiles, dungeonTile,
                                   floorTile, townTile, wallTile)
import           Dungeon.Types    (Dungeon, dungeon)
import           Linear.V2        (V2 (V2))

globalMap :: Dungeon
globalMap = dungeon (stringArrayToMap
    [ "###################"
    , "#.................#"
    , "#.................#"
    , "#.................#"
    , "#.................#"
    , "#.................#"
    , "#........d........#"
    , "#.................#"
    , "#.................#"
    , "#.................#"
    , "#.................#"
    , "#.................#"
    , "#....#............#"
    , "#....#............#"
    , "#....#............#"
    , "#....#............#"
    , "#..t.#............#"
    , "###################"
    ])
    []
    Nothing
    True

stringArrayToMap :: [String] -> TileMap
stringArrayToMap list = allWallTiles (V2 width height) // [(V2 x y, tile c) | (y, row) <- zip [0..] list, (x, c) <- zip [0..] row]
    where tile c
            | c == '#' = wallTile
            | c == '.' = floorTile
            | c == 't' = townTile
            | c == 'd' = dungeonTile
            | otherwise = error "Invalid tile type."
          height = length list
          width = length $ head list
