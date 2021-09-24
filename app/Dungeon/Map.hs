module Dungeon.Map
    ( generate
    ) where

import           Data.Array   (Array, array)
import           Dungeon.Size (height, width)

type Map a = Array (Int, Int) a

generate :: ((Int, Int) -> a) -> Map a
generate f = array ((0, 0), (width - 1, height - 1))
    [((x, y), f (x, y)) | x <- [0 .. width - 1], y <- [0 .. height - 1]]
