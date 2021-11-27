module Dungeon.PathFinder
    ( getPathTo
    ) where

import           Coord            (Coord)
import           Data.Array       (bounds, (!))
import           Data.Graph.AStar (aStar)
import           Data.HashSet     (HashSet, fromList)
import           Dungeon          (Dungeon, walkableFloor)
import           Dungeon.Map.Bool (BoolMap)
import           Dungeon.Map.Tile (TileCollection)
import           Linear.V2        (V2 (..))

getPathTo :: TileCollection -> Dungeon -> Coord -> Coord -> Maybe [Coord]
getPathTo ts d src dst =
    aStar
        (neighbors ts d dst)
        distanceBetween
        (distanceBetween dst)
        (== dst)
        src

neighbors :: TileCollection -> Dungeon -> Coord -> Coord -> HashSet Coord
neighbors ts d dst = fromList . candidate (walkableFloor ts d) dst

distanceBetween :: Coord -> Coord -> Int
distanceBetween p0 p1 =
    let diff = p0 - p1
        V2 x y = diff
     in x * x + y * y

candidate :: BoolMap -> Coord -> Coord -> [Coord]
candidate walkable dst (V2 sx sy)
    -- The destination is *not walkable* as there is an actor. However, it
    -- makes the `aStar` function return `Nothing`. So, we add the
    -- destination as a candidate of next steps to let the function
    -- calculcate the path correctly.
 =
    [ V2 x y
    | x <- [sx - 1 .. sx + 1]
    , y <- [sy - 1 .. sy + 1]
    , (x, y) /= (sx, sy)
    , x >= 0
    , x < width
    , y >= 0
    , y < height
    , walkable ! V2 x y || V2 x y == dst
    ]
  where
    V2 width height = snd (bounds walkable) + V2 1 1
