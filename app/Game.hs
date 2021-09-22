{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Game where

import           Brick                          (AttrName)
import           Control.Lens                   (makeLenses, (%~), (&), (.=),
                                                 (.~), (^.))
import           Control.Monad.Trans.Maybe      (MaybeT (MaybeT), runMaybeT)
import           Control.Monad.Trans.State.Lazy (execState, modify)
import           Coord                          (Coord)
import           Data.Array                     (Array)
import           Data.Array.Base                (array, bounds, elems, (!),
                                                 (//))
import           Dungeon                        (BoolMap, Tile, emptyBoolMap,
                                                 height, initDungeon, width)
import           Graphics.Vty.Attributes.Color  (Color, white, yellow)
import           Linear.V2                      (V2 (..), _x, _y)
import           System.Random.Stateful         (newStdGen)

type Map = Array (Int, Int) Tile

data Game = Game
          { _player   :: Entity
          , _npc      :: Entity
          , _gameMap  :: Map
          , _visible  :: BoolMap
          , _explored :: BoolMap
          } deriving (Show)

data Entity = Entity
            { _position   :: Coord
            , _char       :: String
            , _entityAttr :: AttrName
            } deriving (Show)

makeLenses ''Game
makeLenses ''Entity
makeLenses ''Tile

updateMap :: Game -> Game
updateMap = updateExplored . updateFov

updateExplored :: Game -> Game
updateExplored g = g & explored .~ newExplored
    where newExplored = array ((0, 0), (width - 1, height - 1))
                            [((x, y), (g ^. visible) ! (x, y) || (g ^. explored) ! (x, y)) | x <- [0 .. width - 1], y <- [0 .. height - 1]]

updateFov :: Game -> Game
updateFov g = g & visible .~ calculateFov g

fovRadius :: Int
fovRadius = 8

calculateFov :: Game -> BoolMap
calculateFov Game { _gameMap = m, _player = p } =
        foldl (flip (calculateLos m pos0)) emptyBoolMap
              [V2 (x0 + x) (y0 + y) | x <- [(-fovRadius) .. fovRadius], y <- [(-fovRadius) .. fovRadius]]
        where pos0 = p ^. position
              x0 = pos0 ^. _x
              y0 = pos0 ^. _y

calculateLos :: Map -> Coord -> Coord -> BoolMap -> BoolMap
calculateLos m (V2 x0 y0) (V2 x1 y1) = calculateLosAccum (V2 x0 y0) m (V2 x0 y0) (V2 x1 y1)

calculateLosAccum :: Coord -> Map -> Coord -> Coord -> BoolMap -> BoolMap
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

move :: Direction -> Game -> Game
move d g = flip execState g . runMaybeT $
    MaybeT . fmap Just $ player .= nextPlayer d g

nextPlayer :: Direction -> Game -> Entity
nextPlayer d g@Game { _player = p }
    = let next = nextPosition d g
            in if movable next g
                   then p & position .~ next
                   else p

movable :: Coord -> Game -> Bool
movable c Game { _gameMap = m }
    = m ! (c ^. _x, c ^. _y) ^. walkable

nextPosition :: Direction -> Game -> Coord
nextPosition d Game { _player = p }
    | d == North = p & _position & _y %~ (\y -> min (y + 1) (height - 1))
    | d == South = p & _position & _y %~ (\y -> max (y - 1) 0)
    | d == East  = p & _position & _x %~ (\x -> min (x + 1) (width - 1))
    | d == West  = p & _position & _x %~ (\x -> max (x - 1) 0)
nextPosition _ _ = error "unreachable"

entities :: Game -> [Entity]
entities Game { _player = player, _npc = npc } = [player, npc]

data Direction = North | South | East | West deriving (Eq, Show)

initGame :: IO Game
initGame = do
        gen <- newStdGen
        let xm = width `div` 2
        let ym = height `div` 2
        let (dungeon, playerPos) = initDungeon gen
        let player = Entity { _position = playerPos
                            , _char = "@"
                            , _entityAttr = "playerAttr"
                            }
        let npc = Entity { _position = V2 (xm - 5) ym
                         , _char = "@"
                         , _entityAttr = "npcAttr"
                         }
        let g = Game { _player = player
                     , _npc = npc
                     , _gameMap = dungeon
                     , _visible = emptyBoolMap
                     , _explored = emptyBoolMap
                     }
        return $ updateMap g
