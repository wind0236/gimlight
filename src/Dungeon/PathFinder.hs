module Dungeon.PathFinder
    ( getPathTo
    ) where

import           Coord            (Coord)
import           Data.Array       (bounds, (!))
import           Data.Graph.AStar (aStar)
import           Data.HashSet     (HashSet, fromList)
import           Dungeon          (Dungeon, walkableFloor)
import           Dungeon.Map.Bool (BoolMap)
import           Linear.V2        (V2 (..))

getPathTo :: Dungeon -> Coord -> Coord -> Maybe [Coord]
getPathTo d src dst =
    aStar (neighbors d) distanceBetween (distanceBetween dst) (== dst) src

neighbors :: Dungeon -> Coord -> HashSet Coord
neighbors d src = fromList $ candidate (walkableFloor d) src

distanceBetween :: Coord -> Coord -> Int
distanceBetween p0 p1 =
    let diff = p0 - p1
        V2 x y = diff
     in x * x + y * y

candidate :: BoolMap -> Coord -> [Coord]
candidate walkable (V2 sx sy) =
    [ V2 x y
    | x <- [sx - 1 .. sx + 1]
    , y <- [sy - 1 .. sy + 1]
    , (x, y) /= (sx, sy)
    , x >= 0
    , x < width
    , y >= 0
    , y < height
    , walkable ! V2 x y
    ]
  where
    V2 width height = snd (bounds walkable) + V2 1 1
