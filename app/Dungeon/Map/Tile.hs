{-# LANGUAGE TemplateHaskell #-}

module Dungeon.Map.Tile
    ( TileMap
    , allWallTiles
    , Tile
    , wallTile
    , floorTile
    , walkable
    , transparent
    , imagePath
    ) where

import           Control.Lens.TH (makeLenses)
import           Data.Array      (Array)
import qualified Dungeon.Map     as M

data Tile = Tile
          { _walkable    :: Bool
          , _transparent :: Bool
          , _imagePath   :: String
          } deriving (Show, Ord, Eq)
makeLenses ''Tile

type TileMap = Array (Int, Int) Tile

allWallTiles :: TileMap
allWallTiles = M.generate $ const wallTile

wallTile :: Tile
wallTile = Tile { _walkable = False
                , _transparent = False
                , _imagePath = "images/wall.png"
                }

floorTile :: Tile
floorTile = Tile { _walkable = True
                 , _transparent = True
                 , _imagePath = "images/grass.png"
                 }
