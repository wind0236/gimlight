{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Dungeon.Map.Tile
    ( TileMap
    , allWallTiles
    , Tile
    , isWalkable
    , isTransparent
    , getImagePath
    , wallTile
    , floorTile
    , townTile
    , dungeonTile
    , upStairs
    , downStairs
    ) where

import           Data.Array   (Array)
import           Data.Binary  (Binary)
import           Data.Text    (Text)
import qualified Dungeon.Map  as M
import           GHC.Generics (Generic)
import           Linear.V2    (V2)

data Tile =
    Tile
        { walkable    :: Bool
        , transparent :: Bool
        , imagePath   :: Text
        }
    deriving (Show, Ord, Eq, Generic)

instance Binary Tile

type TileMap = Array (V2 Int) Tile

allWallTiles :: V2 Int -> TileMap
allWallTiles widthAndHeight = M.generate widthAndHeight (const wallTile)

isWalkable :: Tile -> Bool
isWalkable = walkable

isTransparent :: Tile -> Bool
isTransparent = transparent

getImagePath :: Tile -> Text
getImagePath = imagePath

wallTile :: Tile
wallTile =
    Tile {walkable = False, transparent = False, imagePath = "images/wall.png"}

floorTile :: Tile
floorTile =
    Tile {walkable = True, transparent = True, imagePath = "images/grass.png"}

townTile :: Tile
townTile =
    Tile {walkable = True, transparent = True, imagePath = "images/town.png"}

dungeonTile :: Tile
dungeonTile =
    Tile
        { walkable = True
        , transparent = True
        , imagePath = "images/dungeon_chip.png"
        }

upStairs :: Tile
upStairs =
    Tile
        { walkable = True
        , transparent = True
        , imagePath = "images/up_stairs.png"
        }

downStairs :: Tile
downStairs =
    Tile
        { walkable = True
        , transparent = True
        , imagePath = "images/down_stairs.png"
        }
