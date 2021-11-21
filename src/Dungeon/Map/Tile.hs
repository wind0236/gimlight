{-# LANGUAGE DeriveGeneric #-}

module Dungeon.Map.Tile
    ( TileMap
    , Tile
    , TileCollection
    , TileId
    , tile
    , allWallTiles
    , isWalkable
    , isTransparent
    , floorTile
    , upStairs
    , downStairs
    ) where

import           Data.Array   (Array)
import           Data.Binary  (Binary)
import qualified Dungeon.Map  as M
import           GHC.Generics (Generic)
import           Linear.V2    (V2)

type TileId = Int

type TileMap = Array (V2 Int) TileId

data Tile =
    Tile
        { walkable    :: Bool
        , transparent :: Bool
        }
    deriving (Show, Ord, Eq, Generic)

instance Binary Tile

type TileCollection = Array Int Tile

tile :: Bool -> Bool -> Tile
tile = Tile

allWallTiles :: V2 Int -> TileMap
allWallTiles widthAndHeight = M.generate widthAndHeight (const 1)

isWalkable :: Tile -> Bool
isWalkable = walkable

isTransparent :: Tile -> Bool
isTransparent = transparent

floorTile :: TileId
floorTile = 0

upStairs :: TileId
upStairs = 3

downStairs :: TileId
downStairs = 4
