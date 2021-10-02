module Map
    ( generate
    ) where

import           Data.Array   (Array, array)
import           Dungeon.Size (height, width)

generate :: ((Int, Int) -> a) -> Array (Int, Int)  a
generate f = array ((0, 0), (width - 1, height - 1))
    [((x, y), f (x, y)) | x <- [0 .. width - 1], y <- [0 .. height - 1]]
