{-# LANGUAGE DeriveGeneric #-}

module Gimlight.Dungeon.Map.Tile
    ( Tile
    , TileCollection
    , TileId
    , TileIndex
    , tile
    , isWalkable
    , isTransparent
    , getImage
    ) where

import           Codec.Picture        (Image (Image, imageData, imageHeight, imageWidth),
                                       PixelRGBA8)
import           Data.Binary          (Binary (get, put))
import           Data.Map             (Map)
import           Data.Vector.Storable (fromList, toList)
import           GHC.Generics         (Generic)

data Tile =
    Tile
        { walkable    :: Bool
        , transparent :: Bool
        , image       :: Image PixelRGBA8
        }
    deriving (Eq, Generic)

instance Show Tile where
    show t =
        "Tile {walkable = " ++
        show (walkable t) ++
        ", transparent = " ++ show (transparent t) ++ ", ...}"

instance Ord Tile where
    a <= b =
        walkable a <= walkable a &&
        transparent a <= transparent b &&
        imageWidth (image a) <= imageWidth (image b) &&
        imageHeight (image a) <= imageHeight (image b) &&
        imageData (image a) <= imageData (image b)

instance Binary Tile where
    put t = do
        put $ walkable t
        put $ transparent t
        put . imageWidth $ image t
        put . imageHeight $ image t
        put . toList . imageData $ image t
    get =
        Tile <$> get <*> get <*> (Image <$> get <*> get <*> (fromList <$> get))

type TileCollection = Map TileId Tile

type TileId = (FilePath, Int)

type TileIndex = Int

tile :: Bool -> Bool -> Image PixelRGBA8 -> Tile
tile = Tile

getImage :: Tile -> Image PixelRGBA8
getImage = image

isWalkable :: Tile -> Bool
isWalkable = walkable

isTransparent :: Tile -> Bool
isTransparent = transparent
