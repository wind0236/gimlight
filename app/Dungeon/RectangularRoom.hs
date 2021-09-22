module Dungeon.RectangularRoom
    ( RectangularRoom (..)
    , roomFromWidthHeight
    , roomFromTwoPositionInclusive
    ) where

import           Control.Lens.Getter ((^.))
import           Coord               (Coord)
import           Linear.V2           (V2, _x, _y)

data RectangularRoom = RectangularRoom
                      { x1 :: Int
                      , y1 :: Int
                      , x2 :: Int
                      , y2 :: Int
                      }

roomFromWidthHeight :: Coord -> V2 Int -> RectangularRoom
roomFromWidthHeight tl wh = RectangularRoom { x1 = topLeftX
                                             , x2 = topLeftX + roomWidth
                                             , y1 = topLeftY
                                             , y2 = topLeftY + roomHeight
                                             }
                                             where topLeftX = tl ^. _x
                                                   topLeftY = tl ^. _y
                                                   roomWidth = wh ^. _x
                                                   roomHeight = wh ^. _y

roomFromTwoPositionInclusive :: Coord -> Coord -> RectangularRoom
roomFromTwoPositionInclusive pos1 pos2 =
        RectangularRoom { x1 = topLeftX
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
