{-# LANGUAGE OverloadedStrings #-}
module Dungeon where

import           Brick      (AttrName)
import           Data.Array (Array, array, (//))
import           Linear.V2  (V2 (..))

data RecutangularRoom = RecutangularRoom
                      { x1 :: Int
                      , y1 :: Int
                      , x2 :: Int
                      , y2 :: Int
                      }

height, width :: Int
height = 30
width = 80

center :: RecutangularRoom -> V2 Int
center RecutangularRoom{ x1 = x1, y1 = y1, x2 = x2, y2 = y2 }
    = V2 xm ym
    where xm = (x1 + x2) `div` 2
          ym = (y1 + y2) `div` 2

initMap :: Array (Int, Int) Tile
initMap = emptyTiles // [((x, 22), wallTile) | x <- [30 .. 32]]

emptyTiles :: Array (Int, Int) Tile
emptyTiles = array ((0, 0), (width - 1, height - 1))
    [((x, y), floorTile) | x <- [0 .. width - 1], y <- [0 .. height - 1]]

wallTile :: Tile
wallTile = Tile { _walkable = False
            , _transparent = False
            , _tileAttr = "wallAttr"
            }

floorTile :: Tile
floorTile = Tile { _walkable = True
             , _transparent = True
             , _tileAttr = "floorAttr"
             }

data Tile = Tile
          { _walkable    :: Bool
          , _transparent :: Bool
          , _tileAttr    :: AttrName
          } deriving (Show)
