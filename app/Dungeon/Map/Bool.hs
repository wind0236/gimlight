module Dungeon.Map.Bool
    ( BoolMap
    , emptyBoolMap
    ) where

import           Data.Array  (Array)
import qualified Dungeon.Map as M
import           Linear.V2   (V2)

type BoolMap = Array (V2 Int) Bool

emptyBoolMap :: BoolMap
emptyBoolMap = M.generate $ const False
