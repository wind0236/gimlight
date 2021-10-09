module Dungeon.Map
    ( generate
    ) where

import           Data.Array   (Array, array)
import           Dungeon.Size (height, width)
import           Linear.V2    (V2 (V2))

generate :: (V2 Int -> a) -> Array (V2 Int) a
generate f = array (V2 0 0, V2 (width - 1) (height - 1))
    [(V2 x y, f $ V2 x y) | x <- [0 .. width - 1], y <- [0 .. height - 1]]
