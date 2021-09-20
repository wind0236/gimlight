{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Game where

import           Brick                          (AttrName)
import           Control.Lens                   (makeLenses, (%~), (&), (.=),
                                                 (.~), (^.))
import           Control.Monad.Trans.Maybe      (MaybeT (MaybeT), runMaybeT)
import           Control.Monad.Trans.State.Lazy (execState, modify)
import           Data.Array                     (Array)
import           Data.Array.Base                (array, (!), (//))
import           Dungeon                        (Tile, height, initMap, width)
import           Graphics.Vty.Attributes.Color  (Color, white, yellow)
import           Linear.V2                      (V2 (..), _x, _y)

type Coord = V2 Int
type Map = Array (Int, Int) Tile

data Game = Game
          { _player  :: Entity
          , _npc     :: Entity
          , _gameMap :: Map
          } deriving (Show)

data Entity = Entity
            { _position   :: Coord
            , _char       :: [Char]
            , _entityAttr :: AttrName
            } deriving (Show)

makeLenses ''Game
makeLenses ''Entity
makeLenses ''Tile

move :: Direction -> Game -> Game
move d g = flip execState g . runMaybeT $ do
    MaybeT . fmap Just $ player .= nextPlayer d g

nextPlayer :: Direction -> Game -> Entity
nextPlayer d g@Game { _player = p }
    = let next = nextPosition d g
            in if movable next g
                   then p & position .~ next
                   else p

movable :: Coord -> Game -> Bool
movable c Game { _gameMap = m }
    = (m ! (c ^. _x, c ^. _y)) ^. walkable

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
        let xm = width `div` 2
        let ym = height `div` 2
        let player = Entity { _position = V2 xm ym
                            , _char = "@"
                            , _entityAttr = "playerAttr"
                            }
        let npc = Entity { _position = V2 (xm - 5) ym
                         , _char = "@"
                         , _entityAttr = "npcAttr"
                         }
        let g = Game { _player = player
                     , _npc = npc
                     , _gameMap = initMap
                     }
        return g
