module Dungeon.PathFinder
    ( getPathTo
    ) where

import           Control.Monad    (msum)
import           Coord            (Coord (..))
import           Data.Array       ((!), (//))
import           Dungeon          (Dungeon, walkableFloor)
import           Dungeon.Map.Bool (BoolMap, emptyBoolMap)
import qualified Dungeon.Size     as DS
import           Linear.V2        (V2 (..))

getPathTo :: Dungeon -> Coord -> Coord -> [Coord]
getPathTo d src = getPathToAcc emptyBoolMap [src] (walkableFloor d) src

getPathToAcc :: BoolMap -> [Coord] -> BoolMap -> Coord -> Coord -> [Coord]
getPathToAcc visited path walkable src dst
    | src == dst = drop 1 $ reverse path    -- exclude the start point
    | otherwise =  nextStep $ candidate visited walkable src
    where nextStep xs = msum $ map (\c@(V2 x y) -> getPathToAcc (visited // [((x, y), True)]) (src:xs) walkable (V2 x y) dst) $ candidate visited walkable src

candidate :: BoolMap -> BoolMap -> Coord -> [Coord]
candidate visited walkable (V2 sx sy) = [V2 x y |
                              x <- [sx - 1 .. sx + 1],
                              y <- [sy - 1 .. sy + 1],
                              (x, y) /= (sx, sy),
                              x >= 0, x < DS.width,
                              y >= 0, y < DS.height,
                              not $ visited ! (x, y),
                              walkable ! (x, y)
                              ]
