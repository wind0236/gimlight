module Dungeon.Generate.Room
    ( Room (..)
    , center
    , roomFromWidthHeight
    , roomFromTwoPositionInclusive
    , roomOverlaps
    ) where

import           Control.Lens.Getter ((^.))
import           Coord               (Coord)
import           Linear.V2           (V2 (..), _x, _y)

data Room = Room
            { x1 :: Int
            , y1 :: Int
            , x2 :: Int
            , y2 :: Int
            }

center :: Room -> Coord
center r
    = V2 xm ym
    where xm = (x1 r + x2 r) `div` 2
          ym = (y1 r + y2 r) `div` 2

roomOverlaps :: Room -> Room -> Bool
roomOverlaps Room { x1 = aX1, x2 = aX2, y1 = aY1, y2 = aY2 }
             Room { x1 = bX1, x2 = bX2, y1 = bY1, y2 = bY2 }
                = (aX1 <= bX2) && (aX2 >= bX1) && (aY1 <= bY2) && (aY2 >= bY1)

roomFromWidthHeight :: Coord -> V2 Int -> Room
roomFromWidthHeight tl wh = Room { x1 = topLeftX
                                 , x2 = topLeftX + roomWidth
                                 , y1 = topLeftY
                                 , y2 = topLeftY + roomHeight
                                 }
                                 where topLeftX = tl ^. _x
                                       topLeftY = tl ^. _y
                                       roomWidth = wh ^. _x
                                       roomHeight = wh ^. _y

roomFromTwoPositionInclusive :: Coord -> Coord -> Room
roomFromTwoPositionInclusive pos1 pos2 =
        Room { x1 = topLeftX
             , x2 = bottomRightX + 1
             , y1 = topLeftY
             , y2 = bottomRightY + 1
             }
             where pos1X = pos1 ^. _x
                   pos1Y = pos1 ^. _y
                   pos2X = pos2 ^. _x
                   pos2Y = pos2 ^. _y
                   topLeftX = min pos1X pos2X
                   topLeftY = min pos1Y pos2Y
                   bottomRightX = max pos1X pos2X
                   bottomRightY = max pos1Y pos2Y
