module Direction
    ( Direction(North,South,East,West)
    , directionToOffset
    ) where

import           Linear.V2 (V2 (..))

data Direction = North | South | East | West deriving (Eq, Show)

directionToOffset :: Direction -> V2 Int
directionToOffset North = V2 0 1
directionToOffset South = V2 0 (-1)
directionToOffset East  = V2 1 0
directionToOffset West  = V2 (-1) 0
