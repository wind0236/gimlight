{-# LANGUAGE OverloadedStrings #-}
module Dungeon where

import           Brick                   (AttrName)
import           Control.Lens            ((^.))
import           Coord                   (Coord)
import           Data.Array              (Array, array, (//))
import           Dungeon.BoolMap         (BoolMap)
import           Dungeon.GameMap         (GameMap)
import           Dungeon.RectangularRoom (RectangularRoom (..),
                                          roomFromTwoPositionInclusive,
                                          roomFromWidthHeight)
import           Dungeon.Size            (height, width)
import           Dungeon.Tile            (Tile (..), floorTile, wallTile)
import           Linear.V2               (V2 (..), _x, _y)
import           System.Random           (Random (randomR), RandomGen, StdGen,
                                          getStdGen, mkStdGen)

generateDungeon :: StdGen -> Int -> Int -> Int -> V2 Int -> (GameMap, V2 Int, StdGen)
generateDungeon = generateDungeonAccum [] allWallTiles (V2 0 0)

generateDungeonAccum :: [RectangularRoom] -> GameMap -> Coord -> StdGen -> Int -> Int -> Int -> V2 Int -> (GameMap, V2 Int, StdGen)
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

center :: RectangularRoom -> Coord
center RectangularRoom{ x1 = x1, y1 = y1, x2 = x2, y2 = y2 }
    = V2 xm ym
    where xm = (x1 + x2) `div` 2
          ym = (y1 + y2) `div` 2

createRoom :: RectangularRoom -> GameMap -> GameMap
createRoom RectangularRoom{ x1 = x1, y1 = y1, x2 = x2, y2 = y2 } r
    = r // [((x, y), floorTile) | x <- [x1 .. x2 - 1], y <- [y1 .. y2 - 1]]

roomOverlaps :: RectangularRoom -> RectangularRoom -> Bool
roomOverlaps RectangularRoom { x1 = aX1, x2 = aX2, y1 = aY1, y2 = aY2 }
             RectangularRoom { x1 = bX1, x2 = bX2, y1 = bY1, y2 = bY2 }
                = (aX1 <= bX2) && (aX2 >= bX1) && (aY1 <= bY2) && (aY2 >= bY1)

initDungeon :: StdGen -> (GameMap, Coord)
initDungeon gen =
        let (dungeon, playerPos, _) = generateDungeon gen 30 6 10 (V2 width height)
        in (dungeon, playerPos)

tunnelBetween :: Coord -> Coord -> GameMap -> GameMap
tunnelBetween start end d = createRoom path1 $ createRoom path2 d
    where path1 = roomFromTwoPositionInclusive start corner
          path2 = roomFromTwoPositionInclusive corner end
          corner = V2 (start ^. _x) (end ^. _y)

allWallTiles :: GameMap
allWallTiles = array ((0, 0), (width - 1, height - 1))
    [((x, y), wallTile) | x <- [0 .. width - 1], y <- [0 .. height - 1]]
