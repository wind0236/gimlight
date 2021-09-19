{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module Game where

import           Control.Lens                   (makeLenses, (%~), (&), (.=),
                                                 (.~))
import           Control.Monad.Trans.Maybe      (MaybeT (MaybeT), runMaybeT)
import           Control.Monad.Trans.State.Lazy (execState, modify)
import           Graphics.Vty.Attributes.Color  (Color, white)
import           Linear.V2                      (V2 (..), _x, _y)

initGame :: IO Game
initGame = do
        let xm = width `div` 2
        let ym = height `div` 2
        let player = Entity { _position = V2 xm ym
                            , _char = "@"
                            , _color = white
                            }
        let npc = Entity { _position = V2 (xm - 5) ym
                         , _char = "@"
                         , _color = white
                         }
        let g = Game { _player = player
                     , _npc = npc
                     }
        return g


height, width :: Int
height = 30
width = 80

type Coord = V2 Int

data Game = Game
          { _player :: Entity
          , _npc    :: Entity
          } deriving (Show)

data Entity = Entity
            { _position :: Coord
            , _char     :: [Char]
            , _color    :: Color
            } deriving (Show)

makeLenses ''Game
makeLenses ''Entity

move :: Direction -> Game -> Game
move d g = flip execState g . runMaybeT $ do
    MaybeT . fmap Just $ player .= nextPlayer d g

nextPlayer :: Direction -> Game -> Entity
nextPlayer d g@Game { _player = p }
    = p & position .~ nextPosition d g

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
