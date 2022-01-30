{-# LANGUAGE GADTs #-}

module Gimlight.Dungeon.Generate
    ( generateMultipleFloorsDungeon
    ) where

import           Control.Lens                     (Ixed (ix), over, set, (&),
                                                   (.~), (^.))
import           Control.Monad.Morph              (MFunctor (hoist), generalize)
import           Control.Monad.State              (MonadState (get, put),
                                                   MonadTrans (lift), State,
                                                   StateT, execStateT)
import           Data.Either                      (fromRight)
import           Data.Foldable                    (foldlM)
import           Data.Maybe                       (fromMaybe)
import           Data.Tree                        (Tree (Node, rootLabel, subForest))
import           Gimlight.Actor                   (Actor)
import           Gimlight.Actor.Monsters          (orc, troll)
import           Gimlight.Coord                   (Coord)
import           Gimlight.Dungeon                 (Dungeon,
                                                   addAscendingAndDescendingStiars,
                                                   cellMap, dungeon,
                                                   stairsPositionCandidates)
import           Gimlight.Dungeon.Generate.Config (Config, getMapSize,
                                                   getMaxRooms, getNumOfFloors,
                                                   getRoomMaxSize,
                                                   getRoomMinSize)
import           Gimlight.Dungeon.Generate.Room   (Room (..), center,
                                                   roomFromTwoPositionInclusive,
                                                   roomFromWidthHeight,
                                                   roomOverlaps)
import           Gimlight.Dungeon.Identifier      (Identifier)
import           Gimlight.Dungeon.Map.Cell        (CellMap, allWallTiles,
                                                   changeTileAt, locateActorAt,
                                                   locateItemAt,
                                                   tileIdentifierLayer, upper,
                                                   widthAndHeight)
import           Gimlight.Dungeon.Map.Tile        (TileCollection, downStairs,
                                                   upStairs)
import           Gimlight.Dungeon.Stairs          (StairsPair (StairsPair))
import           Gimlight.IndexGenerator          (IndexGenerator)
import           Gimlight.Item                    (herb, sampleBook)
import           Gimlight.TreeZipper              (TreeZipper, appendNode,
                                                   getFocused, goDownBy,
                                                   goToRootAndGetTree, modify,
                                                   treeZipper)
import           Linear.V2                        (V2 (..), _x, _y)
import           System.Random                    (Random (randomR), RandomGen,
                                                   StdGen, random)

generateMultipleFloorsDungeon ::
       TileCollection
    -> Config
    -> Identifier
    -> StateT IndexGenerator (State StdGen) (Tree Dungeon, Coord)
generateMultipleFloorsDungeon ts cfg ident = do
    (firstFloor, ascendingStairsInFirstFloor) <- generateDungeon ts cfg ident
    let treeWithFirstFloor = Node {rootLabel = firstFloor, subForest = []}
        zipperWithFirstFloor = treeZipper treeWithFirstFloor
    dungeonZipper <-
        foldlM
            (\dacc _ -> generateDungeonAndAppend dacc ts cfg ident)
            zipperWithFirstFloor
            [1 .. getNumOfFloors cfg - 1]
    return (goToRootAndGetTree dungeonZipper, ascendingStairsInFirstFloor)

generateDungeonAndAppend ::
       TreeZipper Dungeon
    -> TileCollection
    -> Config
    -> Identifier
    -> StateT IndexGenerator (State StdGen) (TreeZipper Dungeon)
generateDungeonAndAppend zipper ts cfg ident = do
    (generatedDungeon, lowerStairsPosition) <- generateDungeon ts cfg ident
    upperStairsPosition <- lift $ newStairsPosition ts $ getFocused zipper
    let (newUpperDungeon, newLowerDungeon) =
            addAscendingAndDescendingStiars
                (StairsPair upperStairsPosition lowerStairsPosition)
                (getFocused zipper, generatedDungeon)
        newZipper =
            appendNode newLowerDungeon $
            modify
                (const $
                 over
                     cellMap
                     (fromMaybe (error "Failed to change the tile.") .
                      changeTileAt
                          (set upper (Just downStairs))
                          upperStairsPosition)
                     newUpperDungeon)
                zipper
        zipperFocusingNext =
            fromMaybe
                (error "unreachable.")
                (goDownBy (== newLowerDungeon) newZipper)
    return zipperFocusingNext

newStairsPosition :: TileCollection -> Dungeon -> State StdGen Coord
newStairsPosition ts d = do
    index <- randomRST (0, length candidates - 1)
    return $ candidates !! index
  where
    candidates = stairsPositionCandidates ts d

generateDungeon ::
       TileCollection
    -> Config
    -> Identifier
    -> StateT IndexGenerator (State StdGen) (Dungeon, Coord)
generateDungeon tc cfg ident = do
    (tiles, enterPosition) <-
        generateDungeonAccum
            []
            tc
            (allWallTiles $ getMapSize cfg)
            (V2 0 0)
            cfg
            (getMaxRooms cfg)
    return
        ( dungeon
              (fromMaybe (error "Failed to change the tile.") $
               changeTileAt (set upper (Just upStairs)) enterPosition tiles)
              ident
        , enterPosition)

generateDungeonAccum ::
       [Room]
    -> TileCollection
    -> CellMap
    -> Coord
    -> Config
    -> Int
    -> StateT IndexGenerator (State StdGen) (CellMap, V2 Int)
generateDungeonAccum _ _ tileMap playerPos _ 0 = return (tileMap, playerPos)
generateDungeonAccum acc tc tileMap playerPos cfg rooms = do
    roomWidth <- lift $ randomRST (getRoomMinSize cfg, getRoomMaxSize cfg)
    roomHeight <- lift $ randomRST (getRoomMinSize cfg, getRoomMaxSize cfg)
    x <- lift $ randomRST (0, width - roomWidth - 1)
    y <- lift $ randomRST (0, height - roomHeight - 1)
    let room = roomFromWidthHeight (V2 x y) (V2 roomWidth roomHeight)
        appendRoom =
            if null acc
                then createRoom room tileMap
                else tunnelBetween (center room) (center $ head acc) $
                     createRoom room tileMap
    mapWithNewEnemies <- placeEnemies tc appendRoom room maxMonstersPerRoom
    mapWithItems <- lift $ placeItems mapWithNewEnemies tc room maxItemsPerRoom
    let usable = not $ any (roomOverlaps room) acc
        (newMap, newAcc, newPlayerPos) =
            if usable
                then (mapWithItems, room : acc, center room)
                else (tileMap, acc, playerPos)
    generateDungeonAccum newAcc tc newMap newPlayerPos cfg (rooms - 1)
  where
    V2 width height = widthAndHeight tileMap

createRoom :: Room -> CellMap -> CellMap
createRoom room = flip (foldl removeTileAt) coords
  where
    removeTileAt cm x = cm & ix x . tileIdentifierLayer . upper .~ Nothing
    coords =
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
    -> Room
    -> Int
    -> StateT IndexGenerator (State StdGen) CellMap
placeEnemies _ cm _ 0 = return cm
placeEnemies tc cm r n = do
    x <- lift $ randomRST (x1 r, x2 r - 1)
    y <- lift $ randomRST (y1 r, y2 r - 1)
    enemy <- newMonster
    let newMap =
            fromRight cm $ flip execStateT cm $ locateActorAt tc enemy (V2 x y)
    placeEnemies tc newMap r (n - 1)

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

newMonster :: StateT IndexGenerator (State StdGen) Actor
newMonster = do
    r <- lift randomST :: StateT IndexGenerator (State StdGen) Float
    hoist generalize $
        if r < 0.8
            then orc
            else troll

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
