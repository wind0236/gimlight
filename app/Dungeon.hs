{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Dungeon
    ( initDungeon
    , Dungeon
    , updateMap
    , bumpAction
    , entities
    , visible
    , explored
    , gameMap
    ) where

import           Brick                          (AttrName)
import           Control.Lens                   (makeLenses, (%~), (&), (.=),
                                                 (.~), (^.))
import           Control.Monad.Trans.Maybe      (MaybeT (MaybeT), runMaybeT)
import           Control.Monad.Trans.State.Lazy (execState, modify)
import           Coord                          (Coord (..))
import           Data.Array                     (Array)
import           Data.Array.Base                (array, bounds, elems, (!),
                                                 (//))
import           Data.Foldable                  (find)
import           Data.Maybe                     (isJust, isNothing)
import           Direction                      (Direction (East, North, South, West),
                                                 directionToOffset)
import           Dungeon.BoolMap                (BoolMap, emptyBoolMap)
import           Dungeon.GameMap                (GameMap)
import           Dungeon.Generate               (generateDungeon)
import           Dungeon.Room                   (Room (..), x1, x2, y1, y2)
import           Dungeon.Size                   (height, maxRooms, roomMaxSize,
                                                 roomMinSize, width)
import           Dungeon.Tile                   (Tile, darkAttr, lightAttr,
                                                 transparent, walkable)
import           Entity                         (Entity (..), orcEntity,
                                                 playerEntity, position,
                                                 trollEntity)
import           Graphics.Vty.Attributes.Color  (Color, white, yellow)
import           Linear.V2                      (V2 (..), _x, _y)
import           System.Random.Stateful         (StdGen, newStdGen, random,
                                                 randomR)

data Dungeon = Dungeon
          { _player   :: Entity
          , _gameMap  :: GameMap
          , _visible  :: BoolMap
          , _explored :: BoolMap
          , _enemies  :: [Entity]
          } deriving (Show)
makeLenses ''Dungeon

bumpAction :: Direction -> Dungeon -> Dungeon
bumpAction direction dungeon
    | isJust $ getBlockingEntityAtLocation dest dungeon = error "yahoo"
    | otherwise = movePlayer direction dungeon
    where dest = dungeon ^. (player . position) + directionToOffset direction

meleeAction :: Direction -> Dungeon -> Dungeon
meleeAction = undefined

updateMap :: Dungeon -> Dungeon
updateMap = updateExplored . updateFov

updateExplored :: Dungeon -> Dungeon
updateExplored g = g & explored .~ newExplored
    where newExplored = array ((0, 0), (width - 1, height - 1))
                            [((x, y), (g ^. visible) ! (x, y) || (g ^. explored) ! (x, y)) | x <- [0 .. width - 1], y <- [0 .. height - 1]]

updateFov :: Dungeon -> Dungeon
updateFov g = g & visible .~ calculateFov g

fovRadius :: Int
fovRadius = 8

calculateFov :: Dungeon -> BoolMap
calculateFov Dungeon { _gameMap = m, _player = p } =
        foldl (flip (calculateLos m pos0)) emptyBoolMap
              [V2 (x0 + x) (y0 + y) | x <- [(-fovRadius) .. fovRadius], y <- [(-fovRadius) .. fovRadius]]
        where pos0 = p ^. position
              x0 = pos0 ^. _x
              y0 = pos0 ^. _y

calculateLos :: GameMap -> Coord -> Coord -> BoolMap -> BoolMap
calculateLos m (V2 x0 y0) (V2 x1 y1) = calculateLosAccum (V2 x0 y0) m (V2 x0 y0) (V2 x1 y1)

calculateLosAccum :: Coord -> GameMap -> Coord -> Coord -> BoolMap -> BoolMap
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

movePlayer :: Direction -> Dungeon -> Dungeon
movePlayer d g = flip execState g . runMaybeT $
    MaybeT . fmap Just $ player .= nextPlayer d g

nextPlayer :: Direction -> Dungeon -> Entity
nextPlayer d g@Dungeon { _player = p }
    = let next = nextPosition d g
            in if movable next g
                   then p & position .~ next
                   else p

movable :: Coord -> Dungeon -> Bool
movable c d@Dungeon { _gameMap = m }
    = (m ! (c ^. _x, c ^. _y) ^. walkable) && isNothing (getBlockingEntityAtLocation c d)

nextPosition :: Direction -> Dungeon -> Coord
nextPosition d Dungeon { _player = p }
    | d == North = p & _position & _y %~ (\y -> min (y + 1) (height - 1))
    | d == South = p & _position & _y %~ (\y -> max (y - 1) 0)
    | d == East  = p & _position & _x %~ (\x -> min (x + 1) (width - 1))
    | d == West  = p & _position & _x %~ (\x -> max (x - 1) 0)
nextPosition _ _ = error "unreachable"

getBlockingEntityAtLocation :: Coord -> Dungeon -> Maybe Entity
getBlockingEntityAtLocation c d =
        find (\x -> (x ^. position) == c) (d ^. enemies)

entities :: Dungeon -> [Entity]
entities Dungeon { _player = player, _enemies = enemies } = player:enemies

initDungeon :: IO Dungeon
initDungeon = do
        gen <- newStdGen
        let (dungeon, enemies, playerPos, _) = generateDungeon gen maxRooms roomMinSize roomMaxSize (V2 width height)
        let player = playerEntity playerPos
        let g = Dungeon { _player = player
                     , _gameMap = dungeon
                     , _visible = emptyBoolMap
                     , _explored = emptyBoolMap
                     , _enemies = enemies
                     }
        return $ updateMap g
