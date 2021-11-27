module Dungeon.Generate
    ( generateMultipleFloorsDungeon
    ) where

import           Actor                   (Actor)
import           Actor.Monsters          (orc, troll)
import           Control.Lens            ((^.))
import           Coord                   (Coord)
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
import           Dungeon.Map.Cell        (CellMap, allWallTiles, changeTileAt,
                                          locateActorAt, widthAndHeight)
import           Dungeon.Map.Tile        (TileCollection, downStairs, floorTile,
                                          upStairs)
import           Dungeon.Size            (maxSize, minSize)
import           Dungeon.Stairs          (StairsPair (StairsPair))
import           IndexGenerator          (IndexGenerator)
import           Item                    (Item, herb, sampleBook)
import qualified Item                    as I
import           Linear.V2               (V2 (..), _x, _y)
import           System.Random           (Random (randomR), StdGen, random)
import           TreeZipper              (TreeZipper, appendNode, getFocused,
                                          goDownBy, goToRootAndGetTree, modify,
                                          treeZipper)

generateMultipleFloorsDungeon ::
       StdGen
    -> IndexGenerator
    -> TileCollection
    -> Config
    -> Identifier
    -> (Tree Dungeon, Coord, StdGen, IndexGenerator)
generateMultipleFloorsDungeon g ig ts cfg ident =
    (goToRootAndGetTree dungeonZipper, ascendingStairsInFirstFloor, g'', ig'')
  where
    (firstFloor, ascendingStairsInFirstFloor, g', ig') =
        generateDungeon g ig cfg ident
    treeWithFirstFloor = Node {rootLabel = firstFloor, subForest = []}
    zipperWithFirstFloor = treeZipper treeWithFirstFloor
    (dungeonZipper, g'', ig'') =
        foldl
            (\(dacc, gacc, igacc) _ ->
                 generateDungeonAndAppend dacc gacc igacc ts cfg ident)
            (zipperWithFirstFloor, g', ig')
            [1 .. numOfFloors cfg - 1]

generateDungeonAndAppend ::
       TreeZipper Dungeon
    -> StdGen
    -> IndexGenerator
    -> TileCollection
    -> Config
    -> Identifier
    -> (TreeZipper Dungeon, StdGen, IndexGenerator)
generateDungeonAndAppend zipper g ig ts cfg ident =
    (zipperFocusingNext, g'', ig')
  where
    (generatedDungeon, lowerStairsPosition, g', ig') =
        generateDungeon g ig cfg ident
    (upperStairsPosition, g'') = newStairsPosition g' ts $ getFocused zipper
    (newUpperDungeon, newLowerDungeon) =
        addAscendingAndDescendingStiars
            (StairsPair upperStairsPosition lowerStairsPosition)
            (getFocused zipper, generatedDungeon)
    newZipper =
        appendNode newLowerDungeon $
        modify
            (fromMaybe (error "Failed to change the tile.") .
             changeTile upperStairsPosition downStairs) $
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
    -> IndexGenerator
    -> Config
    -> Identifier
    -> (Dungeon, Coord, StdGen, IndexGenerator)
generateDungeon g ig cfg ident =
    ( dungeon
          (fromMaybe (error "Failed to change the tile.") $
           changeTileAt enterPosition upStairs tiles)
          items
          ident
    , enterPosition
    , g'''
    , ig')
  where
    (tiles, items, enterPosition, g''', ig') =
        generateDungeonAccum
            []
            []
            (allWallTiles (V2 width height))
            (V2 0 0)
            g''
            ig
            cfg
    (width, g') = randomR (minSize, maxSize) g
    (height, g'') = randomR (minSize, maxSize) g'

generateDungeonAccum ::
       [Item]
    -> [Room]
    -> CellMap
    -> Coord
    -> StdGen
    -> IndexGenerator
    -> Config
    -> (CellMap, [Item], V2 Int, StdGen, IndexGenerator)
generateDungeonAccum itemsAcc acc tileMap playerPos g ig cfg
    | maxRooms cfg == 0 = (tileMap, itemsAcc, playerPos, g, ig)
    | otherwise =
        generateDungeonAccum
            newItemsAcc
            newAcc
            newMap
            newPlayerPos
            g''''''
            ig'
            cfg {maxRooms = maxRooms cfg - 1}
  where
    (newMap, newItemsAcc, newAcc, newPlayerPos)
        | usable =
            (mapWithNewEnemies, items ++ itemsAcc, room : acc, center room)
        | otherwise = (tileMap, itemsAcc, acc, playerPos)
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
    (mapWithNewEnemies, ig', g''''') =
        placeEnemies appendRoom g'''' ig room maxMonstersPerRoom
    (items, g'''''') = placeItems g''''' room maxItemsPerRoom
    V2 width height = widthAndHeight tileMap

createRoom :: Room -> CellMap -> CellMap
createRoom room r =
    foldl
        (\acc x ->
             fromMaybe
                 (error "Failed to change a tile.")
                 (changeTileAt x floorTile acc))
        r
        [V2 x y | x <- [x1 room .. x2 room - 1], y <- [y1 room .. y2 room - 1]]

tunnelBetween :: Coord -> Coord -> CellMap -> CellMap
tunnelBetween start end d = createRoom path1 $ createRoom path2 d
  where
    path1 = roomFromTwoPositionInclusive start corner
    path2 = roomFromTwoPositionInclusive corner end
    corner = V2 (start ^. _x) (end ^. _y)

placeEnemies ::
       CellMap
    -> StdGen
    -> IndexGenerator
    -> Room
    -> Int
    -> (CellMap, IndexGenerator, StdGen)
placeEnemies cellMap g ig _ 0 = (cellMap, ig, g)
placeEnemies cellMap g ig r n = placeEnemies newMap g''' ig' r (n - 1)
  where
    (x, g') = randomR (x1 r, x2 r - 1) g
    (y, g'') = randomR (y1 r, y2 r - 1) g'
    ((enemy, ig'), g''') = newMonster g'' ig
    newMap = fromMaybe cellMap (locateActorAt enemy (V2 x y) cellMap)

placeItems :: StdGen -> Room -> Int -> ([Item], StdGen)
placeItems = placeItemsAccum []

placeItemsAccum :: [Item] -> StdGen -> Room -> Int -> ([Item], StdGen)
placeItemsAccum items g _ 0 = (items, g)
placeItemsAccum items g r n = placeItemsAccum newItems g''' r (n - 1)
  where
    (x, g') = randomR (x1 r, x2 r - 1) g
    (y, g'') = randomR (y1 r, y2 r - 1) g'
    (prob, g''') = random g'' :: (Float, StdGen)
    newItems
        | V2 x y `notElem` map I.getPosition items = newItem : items
        | otherwise = items
    newItem
        | prob < 0.8 = herb (V2 x y)
        | otherwise = sampleBook (V2 x y)

newMonster :: StdGen -> IndexGenerator -> ((Actor, IndexGenerator), StdGen)
newMonster g ig =
    let (r, g') = random g :: (Float, StdGen)
     in if r < 0.8
            then (orc ig, g')
            else (troll ig, g')

maxMonstersPerRoom :: Int
maxMonstersPerRoom = 1

maxItemsPerRoom :: Int
maxItemsPerRoom = 2
