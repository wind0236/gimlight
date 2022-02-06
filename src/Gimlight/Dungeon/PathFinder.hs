module Gimlight.Dungeon.PathFinder
    ( getPathTo
    ) where

import           Data.Array                (Array, bounds, (!))
import           Data.Graph.AStar          (aStar)
import           Data.HashSet              (HashSet, fromList)
import           Gimlight.Coord            (Coord)
import           Gimlight.Dungeon.Map.Cell (CellMap, walkableFloors)
import           Gimlight.Dungeon.Map.Tile (TileCollection)
import           Linear.V2                 (V2 (..))

getPathTo :: TileCollection -> CellMap -> Coord -> Coord -> Maybe [Coord]
getPathTo ts cm src dst =
    aStar
        (neighbors ts cm dst)
        distanceBetween
        (distanceBetween dst)
        (== dst)
        src

neighbors :: TileCollection -> CellMap -> Coord -> Coord -> HashSet Coord
neighbors ts cm dst = fromList . candidate (walkableFloors ts cm) dst

distanceBetween :: Coord -> Coord -> Int
distanceBetween p0 p1 =
    let diff = p0 - p1
        V2 x y = diff
     in x * x + y * y

candidate :: Array (V2 Int) Bool -> Coord -> Coord -> [Coord]
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
