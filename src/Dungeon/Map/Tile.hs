{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Dungeon.Map.Tile
    ( TileMap
    , allWallTiles
    , Tile
    , wallTile
    , floorTile
    , townTile
    , walkable
    , transparent
    , imagePath
    , dungeonTile
    , upstairs
    ) where

import           Control.Lens.TH (makeLenses)
import           Data.Array      (Array)
import           Data.Binary     (Binary)
import qualified Dungeon.Map     as M
import           GHC.Generics    (Generic)
import           Linear.V2       (V2)

data Tile = Tile
          { _walkable    :: Bool
          , _transparent :: Bool
          , _imagePath   :: String
          } deriving (Show, Ord, Eq, Generic)
makeLenses ''Tile
instance Binary Tile

type TileMap = Array (V2 Int) Tile

allWallTiles :: V2 Int -> TileMap
allWallTiles widthAndHeight = M.generate widthAndHeight (const wallTile)

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

townTile :: Tile
townTile = Tile { _walkable = True
                , _transparent = True
                , _imagePath = "images/town.png"
                }

dungeonTile :: Tile
dungeonTile = Tile { _walkable = True
                   , _transparent = True
                   , _imagePath = "images/dungeon_chip.png"
                   }

upstairs :: Tile
upstairs = Tile { _walkable = True
                , _transparent = True
                , _imagePath = "images/up_stairs.png"
                }
