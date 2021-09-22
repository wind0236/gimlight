module Dungeon.Generate
    ( generateDungeon
    ) where

import           Brick           (AttrName)
import           Control.Lens    ((^.))
import           Coord           (Coord)
import           Data.Array      (Array, array, (//))
import           Dungeon.BoolMap (BoolMap)
import           Dungeon.GameMap (GameMap, allWallTiles)
import           Dungeon.Room    (Room (..), center,
                                  roomFromTwoPositionInclusive,
                                  roomFromWidthHeight, roomOverlaps)
import           Dungeon.Size    (height, width)
import           Dungeon.Tile    (Tile (..), floorTile, wallTile)
import           Entity          (Entity, orcEntity, position, trollEntity)
import           Linear.V2       (V2 (..), _x, _y)
import           System.Random   (Random (randomR), RandomGen, StdGen,
                                  getStdGen, mkStdGen, random)

generateDungeon :: StdGen -> Int -> Int -> Int -> V2 Int -> (GameMap, [Entity], V2 Int, StdGen)
generateDungeon = generateDungeonAccum [] [] allWallTiles (V2 0 0)

generateDungeonAccum :: [Entity] -> [Room] -> GameMap -> Coord -> StdGen -> Int -> Int -> Int -> V2 Int -> (GameMap, [Entity], V2 Int, StdGen)
generateDungeonAccum enemiesAcc _ d pos g 0 _ _ _ = (d, enemiesAcc, pos, g)
generateDungeonAccum enemiesAcc acc dungeon playerPos g maxRooms roomMinSize roomMaxSize mapSize
    = generateDungeonAccum newEnemiesAcc newAcc newDungeon newPlayerPos g''''' (maxRooms - 1) roomMinSize roomMaxSize mapSize
    where (roomWidth, g') = randomR (roomMinSize, roomMaxSize) g
          (roomHeight, g'') = randomR (roomMinSize, roomMaxSize) g'
          (x, g''') = randomR (0, width - roomWidth - 1) g''
          (y, g'''') = randomR (0, height - roomHeight - 1) g'''
          room = roomFromWidthHeight (V2 x y) (V2 roomWidth roomHeight)
          usable = not $ any (roomOverlaps room) acc
          (enemies, g''''') = placeEnemies g'''' room maxMonstersPerRoom
          (newEnemiesAcc, newAcc, newDungeon, newPlayerPos) = if usable
                                                   then if null acc
                                                            then (enemies ++ enemiesAcc, room:acc, createRoom room dungeon, center room)
                                                            else (enemies ++ enemiesAcc, room:acc, tunnelBetween (center room) (center $ head acc) $ createRoom room dungeon, center room)
                                                   else (enemiesAcc, acc, dungeon, playerPos)

createRoom :: Room -> GameMap -> GameMap
createRoom Room{ x1 = x1, y1 = y1, x2 = x2, y2 = y2 } r
    = r // [((x, y), floorTile) | x <- [x1 .. x2 - 1], y <- [y1 .. y2 - 1]]

tunnelBetween :: Coord -> Coord -> GameMap -> GameMap
tunnelBetween start end d = createRoom path1 $ createRoom path2 d
    where path1 = roomFromTwoPositionInclusive start corner
          path2 = roomFromTwoPositionInclusive corner end
          corner = V2 (start ^. _x) (end ^. _y)

maxMonstersPerRoom :: Int
maxMonstersPerRoom = 8

placeEnemies :: StdGen -> Room -> Int -> ([Entity], StdGen)
placeEnemies = placeEnemiesAccum []

placeEnemiesAccum :: [Entity] -> StdGen -> Room -> Int -> ([Entity], StdGen)
placeEnemiesAccum e g _ 0 = (e, g)
placeEnemiesAccum e g r@Room { x1 = x1, x2 = x2, y1 = y1, y2 = y2 } n =
        placeEnemiesAccum newEnemies g''' r (n - 1)
        where (x, g') = randomR (x1, x2) g
              (y, g'') = randomR (y1, y2) g'
              (enemy, g''') = newMonster g'' (V2 x y)
              newEnemies = if V2 x y `notElem` map (^. position) e
                            then enemy:e
                            else e

newMonster :: StdGen -> Coord -> (Entity, StdGen)
newMonster g c =
        let (r, g') = random g :: (Float, StdGen)
        in if r < 0.8
            then (orcEntity c, g')
            else (trollEntity c, g')
