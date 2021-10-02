module Dungeon.PathFinder
    ( getPathTo
    ) where

import           Control.Lens              ((^.))
import           Control.Monad             (msum)
import           Control.Monad.Trans.State (State, state)
import           Coord                     (Coord (..))
import           Data.Array                ((!), (//))
import           Data.Foldable             (minimumBy)
import           Data.Function             (on)
import           Data.Graph.AStar          (aStar)
import           Data.HashSet              (HashSet, fromList)
import           Data.Maybe                (catMaybes, mapMaybe)
import           Dungeon                   (Dungeon, walkableFloor)
import qualified Dungeon.Size              as DS
import           Linear.V2                 (V2 (..))
import           Map.Bool                  (BoolMap, emptyBoolMap)

getPathTo :: Coord -> Coord -> State Dungeon (Maybe [Coord])
getPathTo src dst = state $ \d -> (getPathToNoState d src dst, d)

getPathToNoState :: Dungeon -> Coord -> Coord -> Maybe [Coord]
getPathToNoState d src dst = aStar (neighbors d) distanceBetween (distanceBetween dst) (== dst) src

neighbors :: Dungeon -> Coord -> HashSet Coord
neighbors d src = fromList $ candidate (walkableFloor d) src

distanceBetween :: Coord -> Coord -> Int
distanceBetween p0 p1 = let diff = p0 - p1
                            V2 x y = diff
                        in x * x + y * y

candidate :: BoolMap -> Coord -> [Coord]
candidate walkable (V2 sx sy) = [V2 x y |
                              x <- [sx - 1 .. sx + 1],
                              y <- [sy - 1 .. sy + 1],
                              (x, y) /= (sx, sy),
                              x >= 0, x < DS.width,
                              y >= 0, y < DS.height,
                              walkable ! (x, y)
                              ]
