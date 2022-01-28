module Gimlight.Data.Array
    ( toRowsList
    ) where

import           Data.Array      (Array, assocs, bounds)
import           Data.Foldable   (Foldable (toList))
import           Data.List       (sortBy)
import           Data.List.Split (chunksOf)
import           Linear.V2       (V2 (V2))

toRowsList :: Array (V2 Int) b -> [[b]]
toRowsList arr =
    chunksOf numCols .
    map snd . sortBy (\(V2 _ a, _) (V2 _ b, _) -> compare a b) . toList $
    assocs arr
  where
    V2 numCols _ = upperBound - lowerBound + V2 1 1
    (lowerBound, upperBound) = bounds arr
