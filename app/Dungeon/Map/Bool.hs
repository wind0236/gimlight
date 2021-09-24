module Dungeon.Map.Bool
    ( BoolMap
    , emptyBoolMap
    ) where

import           Data.Array      (Array)
import           Data.Array.Base (array)
import qualified Dungeon.Map     as M
import           Dungeon.Size    (height, width)

type BoolMap = Array (Int, Int) Bool

emptyBoolMap :: BoolMap
emptyBoolMap = M.generate $ const False
