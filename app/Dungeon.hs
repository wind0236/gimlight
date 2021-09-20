{-# LANGUAGE OverloadedStrings #-}
module Dungeon where

import           Brick        (AttrName)
import           Control.Lens ((^.))
import           Data.Array   (Array, array, (//))
import           Linear.V2    (V2 (..), _x, _y)

type Dungeon = Array (Int, Int) Tile

data RecutangularRoom = RecutangularRoom
                      { x1 :: Int
                      , y1 :: Int
                      , x2 :: Int
                      , y2 :: Int
                      }

height, width :: Int
height = 45
width = 80

center :: RecutangularRoom -> V2 Int
center RecutangularRoom{ x1 = x1, y1 = y1, x2 = x2, y2 = y2 }
    = V2 xm ym
    where xm = (x1 + x2) `div` 2
          ym = (y1 + y2) `div` 2

createRoom :: RecutangularRoom -> Dungeon -> Dungeon
createRoom RecutangularRoom{ x1 = x1, y1 = y1, x2 = x2, y2 = y2 } r
    = r // [((x, y), floorTile) | x <- [x1 .. x2 - 1], y <- [y1 .. y2 - 1]]

initDungeon :: Dungeon
initDungeon = tunnelBetween (center room1) (center room2) $ createRoom room1 $ createRoom room2 emptyTiles
    where room1 = roomFromWidthHeight (V2 20 15) (V2 10 15)
          room2 = roomFromWidthHeight (V2 35 15) (V2 10 15)

roomFromWidthHeight :: V2 Int -> V2 Int -> RecutangularRoom
roomFromWidthHeight tl wh = RecutangularRoom { x1 = topLeftX
                                             , x2 = topLeftX + roomWidth
                                             , y1 = topLeftY
                                             , y2 = topLeftY + roomHeight
                                             }
                                             where topLeftX = tl ^. _x
                                                   topLeftY = tl ^. _y
                                                   roomWidth = wh ^. _x
                                                   roomHeight = wh ^. _y

roomFromTwoPositionInclusive :: V2 Int -> V2 Int -> RecutangularRoom
roomFromTwoPositionInclusive topLeft bottomRight =
        RecutangularRoom { x1 = topLeftX
                         , x2 = bottomRightX + 1
                         , y1 = topLeftY
                         , y2 = bottomRightY + 1
                         }
                         where topLeftX = topLeft ^. _x
                               topLeftY = topLeft ^. _y
                               bottomRightX = bottomRight ^. _x
                               bottomRightY = bottomRight ^. _y

tunnelBetween :: V2 Int -> V2 Int -> Dungeon -> Dungeon
tunnelBetween start end d = createRoom path1 $ createRoom path2 d
    where path1 = roomFromTwoPositionInclusive start corner
          path2 = roomFromTwoPositionInclusive corner end
          corner = V2 (start ^. _x) (end ^. _y)

emptyTiles :: Dungeon
emptyTiles = array ((0, 0), (width - 1, height - 1))
    [((x, y), wallTile) | x <- [0 .. width - 1], y <- [0 .. height - 1]]

wallTile :: Tile
wallTile = Tile { _walkable = False
            , _transparent = False
            , _tileAttr = "wallAttr"
            }

floorTile :: Tile
floorTile = Tile { _walkable = True
             , _transparent = True
             , _tileAttr = "floorAttr"
             }

data Tile = Tile
          { _walkable    :: Bool
          , _transparent :: Bool
          , _tileAttr    :: AttrName
          } deriving (Show)
