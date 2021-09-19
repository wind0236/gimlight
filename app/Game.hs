{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module Game where

import           Control.Lens (makeLenses)
import           Linear.V2    (V2 (..))

initGame :: IO Game
initGame = do
        let g = Game {
                     _player = V2 0 0
                     }
        return g


height, width :: Int
height = 80
width = 50

type Coord = V2 Int

data Game = Game
          { _player :: Coord
          } deriving (Show)

makeLenses ''Game
