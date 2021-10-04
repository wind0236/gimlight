module Map.Bool
    ( BoolMap
    , emptyBoolMap
    ) where

import           Data.Array (Array)
import qualified Map        as M

type BoolMap = Array (Int, Int) Bool

emptyBoolMap :: BoolMap
emptyBoolMap = M.generate $ const False
