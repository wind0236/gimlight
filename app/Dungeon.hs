{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Dungeon
    ( initDungeon
    , Dungeon
    , completeThisTurn
    , bumpAction
    , entities
    , visible
    , explored
    , tileMap
    , popPlayer
    ) where

import           Brick                          (AttrName)
import           Control.Lens                   (makeLenses, (%~), (&), (.=),
                                                 (.~), (^.))
import           Control.Monad.Trans.Maybe      (MaybeT (MaybeT), runMaybeT)
import           Control.Monad.Trans.State      (State, state)
import           Control.Monad.Trans.State.Lazy (execState, modify)
import           Coord                          (Coord (..))
import           Data.Array                     (Array)
import           Data.Array.Base                (array, bounds, elems, (!),
                                                 (//))
import           Data.Foldable                  (find)
import           Data.List                      (findIndex)
import           Data.Maybe                     (isJust, isNothing)
import           Direction                      (Direction (East, North, South, West),
                                                 directionToOffset)
import           Dungeon.Generate               (generateDungeon)
import qualified Dungeon.Map                    as M
import           Dungeon.Map.Bool               (BoolMap, emptyBoolMap)
import           Dungeon.Map.Tile               (Tile, TileMap, darkAttr,
                                                 lightAttr, transparent,
                                                 walkable)
import           Dungeon.Room                   (Room (..), x1, x2, y1, y2)
import           Dungeon.Size                   (height, maxRooms, roomMaxSize,
                                                 roomMinSize, width)
import           Entity                         (Entity (..), name, position)
import qualified Entity                         as E
import           Graphics.Vty.Attributes.Color  (Color, white, yellow)
import           Linear.V2                      (V2 (..), _x, _y)
import           Log                            (Message, attackMessage)
import           System.Random.Stateful         (StdGen, newStdGen, random,
                                                 randomR)

data Dungeon = Dungeon
          { _tileMap  :: TileMap
          , _visible  :: BoolMap
          , _explored :: BoolMap
          , _entities :: [Entity]
          } deriving (Show)
makeLenses ''Dungeon

bumpAction :: Entity -> V2 Int -> Dungeon -> (Maybe Message, Dungeon)
bumpAction src offset dungeon
    | isJust $ getBlockingEntityAtLocation dest dungeon =  meleeAction src offset dungeon
    | otherwise = (Nothing, moveAction src offset dungeon)
    where dest = src ^. position + offset

meleeAction :: Entity -> V2 Int -> Dungeon -> (Maybe Message, Dungeon)
meleeAction src offset dungeon =
        (fmap attackMessage entityName, pushEntity dungeon src)
        where pos = src ^. position
              dest = pos + offset
              entity = find (\x -> x ^. position == dest) (dungeon ^. entities)
              entityName = fmap (\x -> "Hello, " ++ x ^. name) entity

completeThisTurn :: State Dungeon [Message]
completeThisTurn = do
        ms <- handleEnemyTurns
        updateMap
        return ms

handleEnemyTurns :: State Dungeon [Message]
handleEnemyTurns = state $ \d@Dungeon{ _entities = entities } -> (map (\x -> attackMessage $ (x ^. name) ++ "'s turn.") entities, d)

updateMap :: State Dungeon ()
updateMap = do
               updateExplored
               updateFov

updateExplored :: State Dungeon ()
updateExplored = state $ \g -> let newExplored = M.generate (\(x, y) -> (g ^. visible) ! (x, y) || (g ^. explored) ! (x, y))
                               in ((), g & explored .~ newExplored)

updateFov :: State Dungeon ()
updateFov = state $ \g -> ((), g & visible .~ calculateFov g)

fovRadius :: Int
fovRadius = 8

calculateFov :: Dungeon -> BoolMap
calculateFov d@Dungeon { _tileMap = m } =
        foldl (flip (calculateLos m pos0)) emptyBoolMap
              [V2 (x0 + x) (y0 + y) | x <- [(-fovRadius) .. fovRadius], y <- [(-fovRadius) .. fovRadius]]
        where pos0 = getPlayerEntity d ^. position
              x0 = pos0 ^. _x
              y0 = pos0 ^. _y

calculateLos :: TileMap -> Coord -> Coord -> BoolMap -> BoolMap
calculateLos m (V2 x0 y0) (V2 x1 y1) = calculateLosAccum (V2 x0 y0) m (V2 x0 y0) (V2 x1 y1)

calculateLosAccum :: Coord -> TileMap -> Coord -> Coord -> BoolMap -> BoolMap
calculateLosAccum (V2 xnext ynext) map (V2 x0 y0) (V2 x1 y1) fov
        | x1 < 0 || y1 < 0 || x1 >= width || y1 >= height = fov
        | V2 xnext ynext == V2 x1 y1 = fov // [((x1, y1), True)]
        | not $ map ! (xnext, ynext) ^. transparent = fov
        | fromIntegral(abs(dy * (xnext - x0 + sx) - dx * (ynext - y0))) / dist < 0.5 =
            calculateLosAccum (V2 (xnext + sx) ynext) map (V2 x0 y0) (V2 x1 y1) fov
        | fromIntegral(abs(dy * (xnext - x0) - dx * (ynext - y0 + sy))) / dist < 0.5 =
            calculateLosAccum (V2 xnext (ynext + sy)) map (V2 x0 y0) (V2 x1 y1) fov
        | otherwise =
            calculateLosAccum (V2 (xnext + sx) (ynext + sy)) map (V2 x0 y0) (V2 x1 y1) fov
            where dx = x1 - x0
                  dy = y1 - y0
                  sx = if x0 < x1 then 1 else -1
                  sy = if y0 < y1 then 1 else -1
                  dist = sqrt $ fromIntegral $ dx * dx + dy * dy :: Float

moveAction :: Entity -> V2 Int -> Dungeon -> Dungeon
moveAction src offset d = pushEntity d $ updatePosition src offset d

updatePosition :: Entity -> V2 Int -> Dungeon -> Entity
updatePosition src offset g
    = let next = nextPosition src offset
      in if movable next g
            then src & position .~ next
            else src

movable :: Coord -> Dungeon -> Bool
movable c d@Dungeon { _tileMap = m }
    = (m ! (c ^. _x, c ^. _y) ^. walkable) && isNothing (getBlockingEntityAtLocation c d)

nextPosition :: Entity -> V2 Int -> Coord
nextPosition src offset =
    max (V2 0 0) $ min (V2 (width - 1) $ height - 1) $ (src ^. position) + offset

getPlayerEntity :: Dungeon -> Entity
getPlayerEntity Dungeon { _entities = entities } = case find E.isPlayer entities of
                                                      Just p -> p
                                                      Nothing -> error "No player entity."

pushEntity :: Dungeon -> Entity -> Dungeon
pushEntity d@Dungeon{ _entities = entities } e = d { _entities = e:entities }

popPlayer :: Dungeon -> (Entity, Dungeon)
popPlayer d@Dungeon{ _entities = entities } = (player, d{ _entities = newEntities})
    where player = case find E.isPlayer entities of
                       Just p  -> p
                       Nothing -> error "No player entity."
          playerIndex = case findIndex E.isPlayer entities of
                            Just index -> index
                            Nothing    -> error "No player entity."
          newEntities = take playerIndex entities ++ drop (playerIndex + 1) entities

getBlockingEntityAtLocation :: Coord -> Dungeon -> Maybe Entity
getBlockingEntityAtLocation c d =
        find (\x -> (x ^. position) == c) (enemies d)

enemies :: Dungeon -> [Entity]
enemies Dungeon { _entities = entities } = filter (not . E.isPlayer) entities

initDungeon :: IO Dungeon
initDungeon = do
        gen <- newStdGen
        let (dungeon, enemies, playerPos, _) = generateDungeon gen maxRooms roomMinSize roomMaxSize (V2 width height)
        let player = E.player playerPos
        let g = Dungeon { _tileMap = dungeon
                        , _visible = emptyBoolMap
                        , _explored = emptyBoolMap
                        , _entities = player:enemies
                        }
        return $ execState updateMap g
