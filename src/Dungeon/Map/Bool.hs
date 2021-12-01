module Dungeon.Map.Bool
    ( BoolMap
    ) where

import           Data.Array (Array)
import           Linear.V2  (V2)

type BoolMap = Array (V2 Int) Bool
