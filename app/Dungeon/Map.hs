module Dungeon.Map
    ( generate
    ) where

import           Data.Array (Array, array)
import           Linear.V2  (V2 (V2))

generate :: V2 Int -> (V2 Int -> a) -> Array (V2 Int) a
generate withAndHeight@(V2 width height) f = array (V2 0 0, withAndHeight - V2 1 1)
    [(V2 x y, f $ V2 x y) | x <- [0 .. width - 1], y <- [0 .. height - 1]]
