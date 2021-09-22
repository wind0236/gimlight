module Dungeon.BoolMap
    ( BoolMap
    , emptyBoolMap
    ) where

import           Data.Array      (Array)
import           Data.Array.Base (array)
import           Dungeon.Size    (height, width)

type BoolMap = Array (Int, Int) Bool

emptyBoolMap :: BoolMap
emptyBoolMap = array ((0, 0), (width - 1, height - 1))
    [((x, y), False) | x <- [0 .. width - 1], y <- [0 .. height - 1]]
