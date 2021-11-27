{-# LANGUAGE DeriveGeneric #-}

module Dungeon.Map.Tile
    ( Tile
    , TileCollection
    , TileId
    , tile
    , isWalkable
    , isTransparent
    , wallTile
    , floorTile
    , upStairs
    , downStairs
    ) where

import           Data.Array   (Array)
import           Data.Binary  (Binary)
import           GHC.Generics (Generic)

type TileId = Int

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

isWalkable :: Tile -> Bool
isWalkable = walkable

isTransparent :: Tile -> Bool
isTransparent = transparent

floorTile :: TileId
floorTile = 0

wallTile :: TileId
wallTile = 1

upStairs :: TileId
upStairs = 3

downStairs :: TileId
downStairs = 4
