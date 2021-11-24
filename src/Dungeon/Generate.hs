module Dungeon.Generate
    ( generateMultipleFloorsDungeon
    ) where

import           Actor                   (Actor)
import qualified Actor                   as A
import           Actor.Monsters          (orc, troll)
import           Control.Lens            ((^.))
import           Coord                   (Coord)
import           Data.Array              (bounds, (//))
import           Data.Maybe              (fromMaybe)
import           Data.Tree               (Tree (Node, rootLabel, subForest))
import           Dungeon                 (Dungeon,
                                          addAscendingAndDescendingStiars,
                                          changeTile, dungeon,
                                          stairsPositionCandidates)
import           Dungeon.Generate.Config (Config (maxRooms, numOfFloors, roomMaxSize, roomMinSize))
import           Dungeon.Generate.Room   (Room (..), center,
                                          roomFromTwoPositionInclusive,
                                          roomFromWidthHeight, roomOverlaps)
import           Dungeon.Identifier      (Identifier)
import           Dungeon.Map.Tile        (TileCollection, TileMap, allWallTiles,
                                          downStairs, floorTile, upStairs)
import           Dungeon.Size            (maxSize, minSize)
import           Dungeon.Stairs          (StairsPair (StairsPair))
import           Item                    (Item, herb, sampleBook)
import qualified Item                    as I
import           Linear.V2               (V2 (..), _x, _y)
import           System.Random           (Random (randomR), StdGen, random)
import           TreeZipper              (TreeZipper, appendNode, getFocused,
                                          goDownBy, goToRootAndGetTree, modify,
                                          treeZipper)

generateMultipleFloorsDungeon ::
       StdGen
    -> TileCollection
    -> Config
    -> Identifier
    -> (Tree Dungeon, Coord, StdGen)
generateMultipleFloorsDungeon g ts cfg ident =
    (goToRootAndGetTree dungeonZipper, ascendingStairsInFirstFloor, g'')
  where
    (firstFloor, ascendingStairsInFirstFloor, g') = generateDungeon g cfg ident
    treeWithFirstFloor = Node {rootLabel = firstFloor, subForest = []}
    zipperWithFirstFloor = treeZipper treeWithFirstFloor
    (dungeonZipper, g'') =
        foldl
            (\acc _ -> uncurry generateDungeonAndAppend acc ts cfg ident)
            (zipperWithFirstFloor, g')
            [1 .. numOfFloors cfg - 1]

generateDungeonAndAppend ::
       TreeZipper Dungeon
    -> StdGen
    -> TileCollection
    -> Config
    -> Identifier
    -> (TreeZipper Dungeon, StdGen)
generateDungeonAndAppend zipper g ts cfg ident = (zipperFocusingNext, g'')
  where
    (generatedDungeon, lowerStairsPosition, g') = generateDungeon g cfg ident
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

generateDungeon :: StdGen -> Config -> Identifier -> (Dungeon, Coord, StdGen)
generateDungeon g cfg ident =
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
            cfg
    (width, g') = randomR (minSize, maxSize) g
    (height, g'') = randomR (minSize, maxSize) g'

generateDungeonAccum ::
       [Item]
    -> [Actor]
    -> [Room]
    -> TileMap
    -> Coord
    -> StdGen
    -> Config
    -> (TileMap, [Actor], [Item], V2 Int, StdGen)
generateDungeonAccum itemsAcc enemiesAcc acc tileMap playerPos g cfg
    | maxRooms cfg == 0 = (tileMap, enemiesAcc, itemsAcc, playerPos, g)
    | otherwise =
        generateDungeonAccum
            newItemsAcc
            newEnemiesAcc
            newAcc
            newDungeon
            newPlayerPos
            g''''''
            cfg {maxRooms = maxRooms cfg - 1}
  where
    (newItemsAcc, newEnemiesAcc, newAcc, newDungeon, newPlayerPos)
        | usable =
            ( items ++ itemsAcc
            , enemies ++ enemiesAcc
            , room : acc
            , appendRoom
            , center room)
        | otherwise = (itemsAcc, enemiesAcc, acc, tileMap, playerPos)
    usable = not $ any (roomOverlaps room) acc
    appendRoom
        | null acc = createRoom room tileMap
        | otherwise =
            tunnelBetween (center room) (center $ head acc) $
            createRoom room tileMap
    (roomWidth, g') = randomR (roomMinSize cfg, roomMaxSize cfg) g
    (roomHeight, g'') = randomR (roomMinSize cfg, roomMaxSize cfg) g'
    (x, g''') = randomR (0, width - roomWidth - 1) g''
    (y, g'''') = randomR (0, height - roomHeight - 1) g'''
    room = roomFromWidthHeight (V2 x y) (V2 roomWidth roomHeight)
    (enemies, g''''') = placeEnemies g'''' room maxMonstersPerRoom
    (items, g'''''') = placeItems g''''' room maxItemsPerRoom
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
