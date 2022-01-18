module Dungeon.Generate
    ( generateMultipleFloorsDungeon
    ) where

import           Actor                   (Actor)
import           Actor.Monsters          (orc, troll)
import           Control.Lens            ((%~), (&), (.~), (?~), (^.))
import           Control.Monad.State     (MonadState (get, put), State,
                                          execStateT)
import           Coord                   (Coord)
import           Data.Either             (fromRight)
import           Data.Foldable           (foldlM)
import           Data.Maybe              (fromMaybe)
import           Data.Tree               (Tree (Node, rootLabel, subForest))
import           Dungeon                 (Dungeon,
                                          addAscendingAndDescendingStiars,
                                          cellMap, dungeon,
                                          stairsPositionCandidates)
import           Dungeon.Generate.Config (Config, getMapSize, getMaxRooms,
                                          getNumOfFloors, getRoomMaxSize,
                                          getRoomMinSize)
import           Dungeon.Generate.Room   (Room (..), center,
                                          roomFromTwoPositionInclusive,
                                          roomFromWidthHeight, roomOverlaps)
import           Dungeon.Identifier      (Identifier)
import           Dungeon.Map.Cell        (CellMap, allWallTiles, changeTileAt,
                                          locateActorAt, locateItemAt, upper,
                                          widthAndHeight)
import           Dungeon.Map.Tile        (TileCollection, downStairs, upStairs)
import           Dungeon.Stairs          (StairsPair (StairsPair))
import           IndexGenerator          (IndexGenerator)
import           Item                    (herb, sampleBook)
import           Linear.V2               (V2 (..), _x, _y)
import           System.Random           (Random (randomR), RandomGen, StdGen,
                                          random)
import           TreeZipper              (TreeZipper, appendNode, getFocused,
                                          goDownBy, goToRootAndGetTree, modify,
                                          treeZipper)

generateMultipleFloorsDungeon ::
       IndexGenerator
    -> TileCollection
    -> Config
    -> Identifier
    -> State StdGen (Tree Dungeon, Coord, IndexGenerator)
generateMultipleFloorsDungeon ig ts cfg ident = do
    (firstFloor, ascendingStairsInFirstFloor, ig') <-
        generateDungeon ts ig cfg ident
    let treeWithFirstFloor = Node {rootLabel = firstFloor, subForest = []}
        zipperWithFirstFloor = treeZipper treeWithFirstFloor
    (dungeonZipper, ig'') <-
        foldlM
            (\(dacc, igacc) _ ->
                 generateDungeonAndAppend dacc igacc ts cfg ident)
            (zipperWithFirstFloor, ig')
            [1 .. getNumOfFloors cfg - 1]
    return (goToRootAndGetTree dungeonZipper, ascendingStairsInFirstFloor, ig'')

generateDungeonAndAppend ::
       TreeZipper Dungeon
    -> IndexGenerator
    -> TileCollection
    -> Config
    -> Identifier
    -> State StdGen (TreeZipper Dungeon, IndexGenerator)
generateDungeonAndAppend zipper ig ts cfg ident = do
    (generatedDungeon, lowerStairsPosition, ig') <-
        generateDungeon ts ig cfg ident
    upperStairsPosition <- newStairsPosition ts $ getFocused zipper
    let (newUpperDungeon, newLowerDungeon) =
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
                      changeTileAt
                          (\tile -> tile & upper ?~ downStairs)
                          upperStairsPosition)) $
            modify (const newUpperDungeon) zipper
        zipperFocusingNext =
            fromMaybe
                (error "unreachable.")
                (goDownBy (== newLowerDungeon) newZipper)
    return (zipperFocusingNext, ig')

newStairsPosition :: TileCollection -> Dungeon -> State StdGen Coord
newStairsPosition ts d = do
    index <- randomRST (0, length candidates - 1)
    return $ candidates !! index
  where
    candidates = stairsPositionCandidates ts d

generateDungeon ::
       TileCollection
    -> IndexGenerator
    -> Config
    -> Identifier
    -> State StdGen (Dungeon, Coord, IndexGenerator)
generateDungeon tc ig cfg ident = do
    (tiles, enterPosition, ig') <-
        generateDungeonAccum
            []
            tc
            (allWallTiles $ getMapSize cfg)
            (V2 0 0)
            ig
            cfg
            (getMaxRooms cfg)
    return
        ( dungeon
              (fromMaybe (error "Failed to change the tile.") $
               changeTileAt
                   (\tile -> tile & upper ?~ upStairs)
                   enterPosition
                   tiles)
              ident
        , enterPosition
        , ig')

generateDungeonAccum ::
       [Room]
    -> TileCollection
    -> CellMap
    -> Coord
    -> IndexGenerator
    -> Config
    -> Int
    -> State StdGen (CellMap, V2 Int, IndexGenerator)
generateDungeonAccum _ _ tileMap playerPos ig _ 0 =
    return (tileMap, playerPos, ig)
generateDungeonAccum acc tc tileMap playerPos ig cfg rooms = do
    roomWidth <- randomRST (getRoomMinSize cfg, getRoomMaxSize cfg)
    roomHeight <- randomRST (getRoomMinSize cfg, getRoomMaxSize cfg)
    x <- randomRST (0, width - roomWidth - 1)
    y <- randomRST (0, height - roomHeight - 1)
    let room = roomFromWidthHeight (V2 x y) (V2 roomWidth roomHeight)
        appendRoom =
            if null acc
                then createRoom room tileMap
                else tunnelBetween (center room) (center $ head acc) $
                     createRoom room tileMap
    (mapWithNewEnemies, ig') <-
        placeEnemies tc appendRoom ig room maxMonstersPerRoom
    mapWithItems <- placeItems mapWithNewEnemies tc room maxItemsPerRoom
    let usable = not $ any (roomOverlaps room) acc
        (newMap, newAcc, newPlayerPos) =
            if usable
                then (mapWithItems, room : acc, center room)
                else (tileMap, acc, playerPos)
    generateDungeonAccum newAcc tc newMap newPlayerPos ig' cfg (rooms - 1)
  where
    V2 width height = widthAndHeight tileMap

createRoom :: Room -> CellMap -> CellMap
createRoom room r =
    foldl
        (\acc x ->
             fromMaybe
                 (error "Failed to change a tile.")
                 (changeTileAt (\tile -> tile & upper .~ Nothing) x acc))
        r
        [V2 x y | x <- [x1 room .. x2 room - 1], y <- [y1 room .. y2 room - 1]]

tunnelBetween :: Coord -> Coord -> CellMap -> CellMap
tunnelBetween start end d = createRoom path1 $ createRoom path2 d
  where
    path1 = roomFromTwoPositionInclusive start corner
    path2 = roomFromTwoPositionInclusive corner end
    corner = V2 (start ^. _x) (end ^. _y)

placeEnemies ::
       TileCollection
    -> CellMap
    -> IndexGenerator
    -> Room
    -> Int
    -> State StdGen (CellMap, IndexGenerator)
placeEnemies _ cm ig _ 0 = return (cm, ig)
placeEnemies tc cm ig r n = do
    x <- randomRST (x1 r, x2 r - 1)
    y <- randomRST (y1 r, y2 r - 1)
    (enemy, ig') <- newMonster ig
    let newMap =
            fromRight cm $ flip execStateT cm $ locateActorAt tc enemy (V2 x y)
    placeEnemies tc newMap ig' r (n - 1)

placeItems :: CellMap -> TileCollection -> Room -> Int -> State StdGen CellMap
placeItems cm _ _ 0 = return cm
placeItems cm tc r n = do
    x <- randomRST (x1 r, x2 r - 1)
    y <- randomRST (y1 r, y2 r - 1)
    prob <- randomST :: State StdGen Float
    let newItem =
            if prob < 0.8
                then herb
                else sampleBook
        newMap =
            fromRight cm $ flip execStateT cm $ locateItemAt tc newItem (V2 x y)
    placeItems newMap tc r (n - 1)

newMonster :: IndexGenerator -> State StdGen (Actor, IndexGenerator)
newMonster ig = do
    r <- randomST :: State StdGen Float
    return $
        if r < 0.8
            then orc ig
            else troll ig

randomRST :: (Random a, RandomGen g) => (a, a) -> State g a
randomRST range = do
    g <- get
    let (v, g') = randomR range g
    put g'
    return v

randomST :: (RandomGen g, Random a) => State g a
randomST = do
    g <- get
    let (v, g') = random g
    put g'
    return v

maxMonstersPerRoom :: Int
maxMonstersPerRoom = 1

maxItemsPerRoom :: Int
maxItemsPerRoom = 2
