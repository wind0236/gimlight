{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Dungeon
    ( initDungeon
    , Dungeon
    , completeThisTurn
    , entities
    , visible
    , explored
    , tileMap
    , popPlayer
    , enemies
    , pushEntity
    , walkableFloor
    ) where

import           Brick                          (AttrName)
import           Control.Lens                   (makeLenses, (%~), (&), (.=),
                                                 (.~), (^.))
import           Control.Lens.Getter            (use)
import           Control.Monad.Trans.Maybe      (MaybeT (MaybeT), runMaybeT)
import           Control.Monad.Trans.State      (State, evalState, runState,
                                                 state)
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

getPlayerEntity :: Dungeon -> Entity
getPlayerEntity Dungeon { _entities = entities } = case find E.isPlayer entities of
                                                      Just p -> p
                                                      Nothing -> error "No player entity."

pushEntity :: Entity -> State Dungeon ()
pushEntity e = state $ \d@Dungeon{ _entities = entities } -> ((), d { _entities = e:entities })

popPlayer :: State Dungeon Entity
popPlayer = state $ \d@Dungeon{ _entities = entities } ->
                let playerIndex = case findIndex E.isPlayer entities of
                                    Just index -> index
                                    Nothing    -> error "No player entity."
                    player = entities !! playerIndex
                    newEntities = take playerIndex entities ++ drop (playerIndex + 1) entities
                in (player, d{ _entities = newEntities })

walkableFloor :: Dungeon -> BoolMap
walkableFloor d = M.generate (\c -> ((d ^. tileMap) ! c) ^. walkable)

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
