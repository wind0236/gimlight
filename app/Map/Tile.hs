{-# LANGUAGE TemplateHaskell #-}

module Map.Tile
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
import           Dungeon.Size    (height, width)
import qualified Map             as M
import           UI.Attrs        (grayAttr, whiteAttr)

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
                , _darkAttr = grayAttr
                , _lightAttr = whiteAttr
                , _char = 'X'
                }

floorTile :: Tile
floorTile = Tile { _walkable = True
                 , _transparent = True
                 , _darkAttr = grayAttr
                 , _lightAttr = whiteAttr
                 , _char = '.'
                 }
