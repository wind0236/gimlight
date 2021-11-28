module Dungeon.Generate
    ( generateMultipleFloorsDungeon
    ) where

import           Actor                   (Actor)
import           Actor.Monsters          (orc, troll)
import           Control.Lens            ((%~), (&), (^.))
import           Coord                   (Coord)
import           Data.Maybe              (fromMaybe)
import           Data.Tree               (Tree (Node, rootLabel, subForest))
import           Dungeon                 (Dungeon,
                                          addAscendingAndDescendingStiars,
                                          cellMap, dungeon,
                                          stairsPositionCandidates)
import           Dungeon.Generate.Config (Config (maxRooms, numOfFloors, roomMaxSize, roomMinSize))
import           Dungeon.Generate.Room   (Room (..), center,
                                          roomFromTwoPositionInclusive,
                                          roomFromWidthHeight, roomOverlaps)
import           Dungeon.Identifier      (Identifier)
import           Dungeon.Map.Cell        (CellMap, allWallTiles, changeTileAt,
                                          locateActorAt, locateItemAt,
                                          widthAndHeight)
import           Dungeon.Map.Tile        (TileCollection, downStairs, floorTile,
                                          upStairs)
import           Dungeon.Size            (maxSize, minSize)
import           Dungeon.Stairs          (StairsPair (StairsPair))
import           IndexGenerator          (IndexGenerator)
import           Item                    (herb, sampleBook)
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
            (\x ->
                 x &
                 cellMap %~
                 (fromMaybe (error "Failed to change the tile.") .
                  changeTileAt upperStairsPosition downStairs)) $
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
          ident
    , enterPosition
    , g'''
    , ig')
  where
    (tiles, enterPosition, g''', ig') =
        generateDungeonAccum
            []
            (allWallTiles (V2 width height))
            (V2 0 0)
            g''
            ig
            cfg
    (width, g') = randomR (minSize, maxSize) g
    (height, g'') = randomR (minSize, maxSize) g'

generateDungeonAccum ::
       [Room]
    -> CellMap
    -> Coord
    -> StdGen
    -> IndexGenerator
    -> Config
    -> (CellMap, V2 Int, StdGen, IndexGenerator)
generateDungeonAccum acc tileMap playerPos g ig cfg
    | maxRooms cfg == 0 = (tileMap, playerPos, g, ig)
    | otherwise =
        generateDungeonAccum
            newAcc
            newMap
            newPlayerPos
            g''''''
            ig'
            cfg {maxRooms = maxRooms cfg - 1}
  where
    (newMap, newAcc, newPlayerPos)
        | usable = (mapWithItems, room : acc, center room)
        | otherwise = (tileMap, acc, playerPos)
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
    (mapWithItems, g'''''') =
        placeItems mapWithNewEnemies g''''' room maxItemsPerRoom
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
placeEnemies cm g ig _ 0 = (cm, ig, g)
placeEnemies cm g ig r n = placeEnemies newMap g''' ig' r (n - 1)
  where
    (x, g') = randomR (x1 r, x2 r - 1) g
    (y, g'') = randomR (y1 r, y2 r - 1) g'
    ((enemy, ig'), g''') = newMonster g'' ig
    newMap = fromMaybe cm (locateActorAt enemy (V2 x y) cm)

placeItems :: CellMap -> StdGen -> Room -> Int -> (CellMap, StdGen)
placeItems = placeItemsAccum

placeItemsAccum :: CellMap -> StdGen -> Room -> Int -> (CellMap, StdGen)
placeItemsAccum cm g _ 0 = (cm, g)
placeItemsAccum cm g r n = placeItemsAccum newMap g''' r (n - 1)
  where
    newMap = fromMaybe cm (locateItemAt newItem (V2 x y) cm)
    (x, g') = randomR (x1 r, x2 r - 1) g
    (y, g'') = randomR (y1 r, y2 r - 1) g'
    (prob, g''') = random g'' :: (Float, StdGen)
    newItem
        | prob < 0.8 = herb
        | otherwise = sampleBook

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
