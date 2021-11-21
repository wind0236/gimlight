module Dungeon.Generate
    ( generateMultipleFloorsDungeon
    ) where

import           Actor                 (Actor)
import qualified Actor                 as A
import           Actor.Monsters        (orc, troll)
import           Control.Lens          ((^.))
import           Coord                 (Coord)
import           Data.Array            (bounds, (//))
import           Data.Maybe            (fromMaybe)
import           Data.Tree             (Tree (Node, rootLabel, subForest))
import           Dungeon               (Dungeon,
                                        addAscendingAndDescendingStiars,
                                        changeTile, dungeon,
                                        stairsPositionCandidates)
import           Dungeon.Generate.Room (Room (..), center,
                                        roomFromTwoPositionInclusive,
                                        roomFromWidthHeight, roomOverlaps)
import           Dungeon.Identifier    (Identifier)
import           Dungeon.Map.Tile      (TileCollection, TileMap, allWallTiles,
                                        downStairs, floorTile, upStairs)
import           Dungeon.Size          (maxSize, minSize)
import           Dungeon.Stairs        (StairsPair (StairsPair))
import           Item                  (Item, herb, sampleBook)
import qualified Item                  as I
import           Linear.V2             (V2 (..), _x, _y)
import           System.Random         (Random (randomR), StdGen, random)
import           TreeZipper            (TreeZipper, appendNode, getFocused,
                                        goDownBy, goToRootAndGetTree, modify,
                                        treeZipper)

generateMultipleFloorsDungeon ::
       StdGen
    -> TileCollection
    -> Int
    -> Int
    -> Int
    -> Int
    -> V2 Int
    -> Identifier
    -> (Tree Dungeon, Coord, StdGen)
generateMultipleFloorsDungeon g ts floorsNum maxRooms roomMinSize roomMaxSize mapSize ident =
    (goToRootAndGetTree dungeonZipper, ascendingStairsInFirstFloor, g'')
  where
    (firstFloor, ascendingStairsInFirstFloor, g') =
        generateDungeon g maxRooms roomMinSize roomMaxSize mapSize ident
    treeWithFirstFloor = Node {rootLabel = firstFloor, subForest = []}
    zipperWithFirstFloor = treeZipper treeWithFirstFloor
    (dungeonZipper, g'') =
        foldl
            (\acc _ ->
                 uncurry
                     generateDungeonAndAppend
                     acc
                     ts
                     maxRooms
                     roomMinSize
                     roomMaxSize
                     mapSize
                     ident)
            (zipperWithFirstFloor, g')
            [1 .. floorsNum - 1]

generateDungeonAndAppend ::
       TreeZipper Dungeon
    -> StdGen
    -> TileCollection
    -> Int
    -> Int
    -> Int
    -> V2 Int
    -> Identifier
    -> (TreeZipper Dungeon, StdGen)
generateDungeonAndAppend zipper g ts maxRooms roomMinSize roomMaxSize mapSize ident =
    (zipperFocusingNext, g'')
  where
    (generatedDungeon, lowerStairsPosition, g') =
        generateDungeon g maxRooms roomMinSize roomMaxSize mapSize ident
    (upperStairsPosition, g'') = newStairsPosition g' ts $ getFocused zipper
    (newUpperDungeon, newLowerDungeon) =
        addAscendingAndDescendingStiars
            (StairsPair upperStairsPosition lowerStairsPosition)
            (getFocused zipper, generatedDungeon)
    newZipper =
        appendNode newLowerDungeon $
        modify (changeTile upperStairsPosition downStairs) $
        modify (const newUpperDungeon) zipper
    zipperFocusingNext =
        fromMaybe
            (error "unreachable.")
            (goDownBy (== newLowerDungeon) newZipper)

newStairsPosition :: StdGen -> TileCollection -> Dungeon -> (Coord, StdGen)
newStairsPosition g ts d = (candidates !! index, g')
  where
    candidates = stairsPositionCandidates ts d
    (index, g') = randomR (0, length candidates - 1) g

generateDungeon ::
       StdGen
    -> Int
    -> Int
    -> Int
    -> V2 Int
    -> Identifier
    -> (Dungeon, Coord, StdGen)
generateDungeon g maxRooms roomMinSize roomMaxSize mapSize ident =
    ( dungeon (tiles // [(enterPosition, upStairs)]) actors items ident
    , enterPosition
    , g''')
  where
    (tiles, actors, items, enterPosition, g''') =
        generateDungeonAccum
            []
            []
            []
            (allWallTiles (V2 width height))
            (V2 0 0)
            g''
            maxRooms
            roomMinSize
            roomMaxSize
            mapSize
    (width, g') = randomR (minSize, maxSize) g
    (height, g'') = randomR (minSize, maxSize) g'

generateDungeonAccum ::
       [Item]
    -> [Actor]
    -> [Room]
    -> TileMap
    -> Coord
    -> StdGen
    -> Int
    -> Int
    -> Int
    -> V2 Int
    -> (TileMap, [Actor], [Item], V2 Int, StdGen)
generateDungeonAccum itemsAcc enemiesAcc _ d pos g 0 _ _ _ =
    (d, enemiesAcc, itemsAcc, pos, g)
generateDungeonAccum itemsAcc enemiesAcc acc tileMap playerPos g maxRooms roomMinSize roomMaxSize mapSize =
    generateDungeonAccum
        newItemsAcc
        newEnemiesAcc
        newAcc
        newDungeon
        newPlayerPos
        g''''''
        (maxRooms - 1)
        roomMinSize
        roomMaxSize
        mapSize
  where
    (roomWidth, g') = randomR (roomMinSize, roomMaxSize) g
    (roomHeight, g'') = randomR (roomMinSize, roomMaxSize) g'
    (x, g''') = randomR (0, width - roomWidth - 1) g''
    (y, g'''') = randomR (0, height - roomHeight - 1) g'''
    room = roomFromWidthHeight (V2 x y) (V2 roomWidth roomHeight)
    usable = not $ any (roomOverlaps room) acc
    (enemies, g''''') = placeEnemies g'''' room maxMonstersPerRoom
    (items, g'''''') = placeItems g''''' room maxItemsPerRoom
    (newItemsAcc, newEnemiesAcc, newAcc, newDungeon, newPlayerPos) =
        if usable
            then if null acc
                     then ( items ++ itemsAcc
                          , enemies ++ enemiesAcc
                          , room : acc
                          , createRoom room tileMap
                          , center room)
                     else ( items ++ itemsAcc
                          , enemies ++ enemiesAcc
                          , room : acc
                          , tunnelBetween (center room) (center $ head acc) $
                            createRoom room tileMap
                          , center room)
            else (itemsAcc, enemiesAcc, acc, tileMap, playerPos)
    V2 width height = snd (bounds tileMap) + V2 1 1

createRoom :: Room -> TileMap -> TileMap
createRoom room r =
    r //
    [ (V2 x y, floorTile)
    | x <- [x1 room .. x2 room - 1]
    , y <- [y1 room .. y2 room - 1]
    ]

tunnelBetween :: Coord -> Coord -> TileMap -> TileMap
tunnelBetween start end d = createRoom path1 $ createRoom path2 d
  where
    path1 = roomFromTwoPositionInclusive start corner
    path2 = roomFromTwoPositionInclusive corner end
    corner = V2 (start ^. _x) (end ^. _y)

placeEnemies :: StdGen -> Room -> Int -> ([Actor], StdGen)
placeEnemies = placeEnemiesAccum []

placeEnemiesAccum :: [Actor] -> StdGen -> Room -> Int -> ([Actor], StdGen)
placeEnemiesAccum e g _ 0 = (e, g)
placeEnemiesAccum e g r n = placeEnemiesAccum newEnemies g''' r (n - 1)
  where
    (x, g') = randomR (x1 r, x2 r - 1) g
    (y, g'') = randomR (y1 r, y2 r - 1) g'
    (enemy, g''') = newMonster g'' (V2 x y)
    newEnemies =
        if V2 x y `notElem` map (^. A.position) e
            then enemy : e
            else e

placeItems :: StdGen -> Room -> Int -> ([Item], StdGen)
placeItems = placeItemsAccum []

placeItemsAccum :: [Item] -> StdGen -> Room -> Int -> ([Item], StdGen)
placeItemsAccum items g _ 0 = (items, g)
placeItemsAccum items g r n = placeItemsAccum newItems g''' r (n - 1)
  where
    (x, g') = randomR (x1 r, x2 r - 1) g
    (y, g'') = randomR (y1 r, y2 r - 1) g'
    (prob, g''') = random g'' :: (Float, StdGen)
    newItems =
        if V2 x y `notElem` map I.getPosition items
            then if prob < 0.8
                     then herb (V2 x y) : items
                     else sampleBook (V2 x y) : items
            else items

newMonster :: StdGen -> Coord -> (Actor, StdGen)
newMonster g c =
    let (r, g') = random g :: (Float, StdGen)
     in if r < 0.8
            then (orc c, g')
            else (troll c, g')

maxMonstersPerRoom :: Int
maxMonstersPerRoom = 1

maxItemsPerRoom :: Int
maxItemsPerRoom = 2
