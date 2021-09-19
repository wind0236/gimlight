{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module Game where

import           Control.Lens                   (makeLenses, (%~), (&), (.=),
                                                 (.~))
import           Control.Monad.Trans.Maybe      (MaybeT (MaybeT), runMaybeT)
import           Control.Monad.Trans.State.Lazy (execState, modify)
import           Linear.V2                      (V2 (..), _x, _y)

initGame :: IO Game
initGame = do
        let xm = width `div` 2
        let ym = height `div` 2
        let g = Game {
                     _player = V2 xm ym
                     }
        return g


height, width :: Int
height = 30
width = 80

type Coord = V2 Int

data Game = Game
          { _player :: Coord
          } deriving (Show)

makeLenses ''Game

move :: Direction -> Game -> Game
move d g = flip execState g . runMaybeT $ do
    MaybeT . fmap Just $ player .= nextPosition d g

nextPosition :: Direction -> Game -> Coord
nextPosition d Game { _player = p }
    | d == North = p & _y %~ (\y -> min (y + 1) (height - 1))
    | d == South = p & _y %~ (\y -> max (y - 1) 0)
    | d == East  = p & _x %~ (\x -> min (x + 1) (width - 1))
    | d == West  = p & _x %~ (\x -> max (x - 1) 0)
nextPosition _ _ = error "unreachable"

data Direction = North | South | East | West deriving (Eq, Show)
