module Map.Bool
    ( BoolMap
    , emptyBoolMap
    ) where

import           Data.Array      (Array)
import           Data.Array.Base (array)
import           Dungeon.Size    (height, width)
import qualified Map             as M

type BoolMap = Array (Int, Int) Bool

emptyBoolMap :: BoolMap
emptyBoolMap = M.generate $ const False
