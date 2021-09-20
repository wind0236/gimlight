{-# LANGUAGE OverloadedStrings #-}
module Dungeon where

import           Brick         (AttrName)
import           Control.Lens  ((^.))
import           Data.Array    (Array, array, (//))
import           Linear.V2     (V2 (..), _x, _y)
import           System.Random (Random (randomR), RandomGen, StdGen, getStdGen,
                                mkStdGen)

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

generateDungeon :: StdGen -> Int -> Int -> Int -> V2 Int -> (Dungeon, V2 Int, StdGen)
generateDungeon = generateDungeonAccum [] emptyTiles (V2 0 0)

generateDungeonAccum :: [RecutangularRoom] -> Dungeon -> V2 Int -> StdGen -> Int -> Int -> Int -> V2 Int -> (Dungeon, V2 Int, StdGen)
generateDungeonAccum _ d pos g 0 _ _ _ = (d, pos, g)
generateDungeonAccum acc dungeon playerPos g maxRoms roomMinSize roomMaxSize mapSize
    = generateDungeonAccum newAcc newDungeon newPlayerPos g'''' (maxRoms - 1) roomMinSize roomMaxSize mapSize
    where (roomWidth, g') = randomR (roomMinSize, roomMaxSize) g
          (roomHeight, g'') = randomR (roomMinSize, roomMaxSize) g'
          (x, g''') = randomR (0, width - roomWidth - 1) g''
          (y, g'''') = randomR (0, height - roomHeight - 1) g'''
          room = roomFromWidthHeight (V2 x y) (V2 roomWidth roomHeight)
          usable = not $ any (roomOverlaps room) acc
          (newAcc, newDungeon, newPlayerPos) = if usable
                                                   then if null acc
                                                            then (room:acc, createRoom room dungeon, center room)
                                                            else (room:acc, tunnelBetween (center room) (center $ head acc) $ createRoom room dungeon, center room)
                                                   else (acc, dungeon, playerPos)

center :: RecutangularRoom -> V2 Int
center RecutangularRoom{ x1 = x1, y1 = y1, x2 = x2, y2 = y2 }
    = V2 xm ym
    where xm = (x1 + x2) `div` 2
          ym = (y1 + y2) `div` 2

createRoom :: RecutangularRoom -> Dungeon -> Dungeon
createRoom RecutangularRoom{ x1 = x1, y1 = y1, x2 = x2, y2 = y2 } r
    = r // [((x, y), floorTile) | x <- [x1 .. x2 - 1], y <- [y1 .. y2 - 1]]

roomOverlaps :: RecutangularRoom -> RecutangularRoom -> Bool
roomOverlaps RecutangularRoom { x1 = aX1, x2 = aX2, y1 = aY1, y2 = aY2 }
             RecutangularRoom { x1 = bX1, x2 = bX2, y1 = bY1, y2 = bY2 }
                = (aX1 <= bX2) && (aX2 >= bX1) && (aY1 <= bY2) && (aY2 >= bY1)

initDungeon :: StdGen -> (Dungeon, V2 Int)
initDungeon gen =
        let (dungeon, pos, _) = generateDungeon gen 30 6 10 (V2 width height)
        in (dungeon, pos)

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
roomFromTwoPositionInclusive pos1 pos2 =
        RecutangularRoom { x1 = topLeftX
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
