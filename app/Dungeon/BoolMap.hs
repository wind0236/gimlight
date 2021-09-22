module Dungeon.BoolMap
    ( BoolMap
    ) where

import           Data.Array (Array)

type BoolMap = Array (Int, Int) Bool
