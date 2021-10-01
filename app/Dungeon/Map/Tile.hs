{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Dungeon.Map.Tile
    ( TileMap
    , allWallTiles
    , Tile
    , wallTile
    , floorTile
    , walkable
    , transparent
    , darkAttr
    , lightAttr
    , char
    ) where

import           Brick.AttrMap   (AttrName)
import           Control.Lens.TH (makeLenses)
import           Data.Array      (Array)
import           Data.Array.Base (array)
import qualified Dungeon.Map     as M
import           Dungeon.Size    (height, width)

data Tile = Tile
          { _walkable    :: Bool
          , _transparent :: Bool
          , _darkAttr    :: AttrName
          , _lightAttr   :: AttrName
          , _char        :: Char
          } deriving (Show)
makeLenses ''Tile

type TileMap = Array (Int, Int) Tile

allWallTiles :: TileMap
allWallTiles = M.generate $ const wallTile

wallTile :: Tile
wallTile = Tile { _walkable = False
                , _transparent = False
                , _darkAttr = "darkWallAttr"
                , _lightAttr = "lightWallAttr"
                , _char = 'X'
                }

floorTile :: Tile
floorTile = Tile { _walkable = True
                 , _transparent = True
                 , _darkAttr = "darkFloorAttr"
                 , _lightAttr = "lightFloorAttr"
                 , _char = '.'
                 }
