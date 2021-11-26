{-# LANGUAGE DeriveGeneric #-}

module Dungeon.Map.Tile
    ( TileMap
    , tileMap
    , widthAndHeight
    , changeTileAt
    , walkableMap
    , transparentMap
    , tileIdAt
    , isWalkableAt
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

import           Coord            (Coord)
import           Data.Array       (Array, bounds, (!), (//))
import           Data.Binary      (Binary)
import           Data.Maybe       (isJust)
import qualified Dungeon.Map      as M
import           Dungeon.Map.Bool (BoolMap)
import           GHC.Generics     (Generic)
import           Linear.V2        (V2 (V2))

type TileId = Int

newtype TileMap =
    TileMap (Array (V2 Int) TileId)
    deriving (Show, Ord, Eq, Generic)

instance Binary TileMap

tileMap :: Array (V2 Int) TileId -> TileMap
tileMap = TileMap

widthAndHeight :: TileMap -> V2 Int
widthAndHeight (TileMap m) = snd (bounds m) + V2 1 1

changeTileAt :: Coord -> TileId -> TileMap -> Maybe TileMap
changeTileAt c i (TileMap m)
    | isJust $ tileIdAt c (TileMap m) = Just $ TileMap $ m // [(c, i)]
    | otherwise = Nothing

walkableMap :: TileCollection -> TileMap -> BoolMap
walkableMap tc (TileMap m) = isWalkable . (tc !) <$> m

transparentMap :: TileCollection -> TileMap -> BoolMap
transparentMap tc (TileMap m) = isTransparent . (tc !) <$> m

tileIdAt :: Coord -> TileMap -> Maybe TileId
tileIdAt c (TileMap m)
    | c >= lower && c <= upper = Just $ m ! c
    | otherwise = Nothing
  where
    (lower, upper) = bounds m

isWalkableAt :: Coord -> TileCollection -> TileMap -> Bool
isWalkableAt c tc t =
    case tileIdAt c t of
        Just x  -> isWalkable (tc ! x)
        Nothing -> False

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
allWallTiles wh = TileMap $ M.generate wh (const 1)

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
