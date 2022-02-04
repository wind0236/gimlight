module Gimlight.SetUp.TileFile
  ( tilesInUnitedTileFile,
    tilesInSingleTileFile,
    tilesInUnwalkableTileFile,
    haskellTile,
    generateTile,
    unitedTileFile,
    singleTileFile,
    unwalkableTileFile,
    haskellTilePath,
    tileFileForGeneration,
    tileWithoutProperties,
  )
where

import Codec.Picture (Image (imageData), PixelRGBA8)
import Codec.Picture.Extra (flipHorizontally, flipVertically)
import Data.Bits (Bits (bit, (.|.)))
import Data.Foldable (foldlM)
import Data.List (transpose)
import Data.List.Split (chunksOf)
import Data.Map (fromList)
import qualified Data.Vector.Storable as V
import Gimlight.Dungeon.Map.Tile (Tile, TileCollection, TileId, tile)
import Gimlight.SetUp.ImageFile
  ( generateTileImage,
    haskellTileImage,
    singleTileImage,
  )
import Gimlight.UI.Draw.Config (tileWidth)

tilesInUnitedTileFile :: IO TileCollection
tilesInUnitedTileFile = fromList <$> foldlM foldStep [] [0 .. 5]
  where
    foldStep :: [(TileId, Tile)] -> Int -> IO [(TileId, Tile)]
    foldStep acc x =
      fmap
        ((acc ++) . tileList unitedTileFile x (tileOfIndex x))
        (singleTileImage x)
    tileOfIndex n
      | n == unwalkableAndUntransparentTile = tile False False
      | otherwise = tile True True
    unwalkableAndUntransparentTile = 2

tilesInSingleTileFile :: IO TileCollection
tilesInSingleTileFile =
  fromList . tileList singleTileFile 0 (tile True True) <$> singleTileImage 0

tilesInUnwalkableTileFile :: IO TileCollection
tilesInUnwalkableTileFile =
  fromList . tileList unwalkableTileFile 0 (tile False True)
    <$> singleTileImage 0

haskellTile :: IO TileCollection
haskellTile =
  fmap
    (fromList . tileList haskellTilePath 0 (tile True True))
    haskellTileImage

generateTile :: IO TileCollection
generateTile = fromList <$> foldlM foldStep [] [0 .. 29]
  where
    foldStep :: [(TileId, Tile)] -> Int -> IO [(TileId, Tile)]
    foldStep acc x =
      fmap
        ((acc ++) . tileList tileFileForGeneration x (tileOfIndex x))
        (generateTileImage x)
    tileOfIndex n
      | n `elem` unwalkableAndUntransparentTiles = tile False False
      | otherwise = tile True True
    unwalkableAndUntransparentTiles = [1 .. 21]

unitedTileFile :: FilePath
unitedTileFile = "tests/tiles/valid/united.json"

singleTileFile :: FilePath
singleTileFile = "tests/tiles/valid/single.json"

unwalkableTileFile :: FilePath
unwalkableTileFile = "tests/tiles/valid/unwalkable.json"

haskellTilePath :: FilePath
haskellTilePath = "tests/tiles/valid/haskell.json"

tileWithoutProperties :: FilePath
tileWithoutProperties = "tests/tiles/invalid/no_properties.json"

tileFileForGeneration :: FilePath
tileFileForGeneration = "tests/tiles/valid/generate.json"

-- Transformation order is important. Tiled's specification says
--
-- > When rendering an orthographic or isometric tile, the order of
--   operations matters. The diagonal flip is done first, followed by the
--   horizontal and vertical flips. The diagonal flip should flip the
--   bottom left and top right corners of the tile, and can be thought of
--   as an x/y axis swap. For hexagonal tiles, the order does not matter.
--
-- See: https://docs.mapeditor.org/en/stable/reference/global-tile-ids/#gid-tile-flipping
tileList ::
  FilePath ->
  Int ->
  (Image PixelRGBA8 -> Tile) ->
  Image PixelRGBA8 ->
  [(TileId, Tile)]
tileList path idx tileGen img =
  fmap
    (identifierAndTileForDVH path idx tileGen img)
    diagonalVertialHorizontal

identifierAndTileForDVH ::
  FilePath ->
  Int ->
  (Image PixelRGBA8 -> Tile) ->
  Image PixelRGBA8 ->
  (Bool, Bool, Bool) ->
  (TileId, Tile)
identifierAndTileForDVH path idx tileGen img (d, v, h) =
  ((path, tileFlagsSetter d v h idx), tileGen (transformImage d v h img))

transformImage :: Bool -> Bool -> Bool -> Image PixelRGBA8 -> Image PixelRGBA8
transformImage d v h =
  applyFunctionWhen h flipHorizontally
    . applyFunctionWhen v flipVertically
    . applyFunctionWhen d swapImageXY
  where
    applyFunctionWhen cond f =
      if cond
        then f
        else id

swapImageXY :: Image PixelRGBA8 -> Image PixelRGBA8
swapImageXY img =
  img
    { imageData =
        V.fromList
          ( concat $
              concat $
                transpose $
                  chunksOf tileWidth $ chunksOf 4 $ V.toList $ imageData img
          )
    }

tileFlagsSetter :: Bool -> Bool -> Bool -> Int -> Int
tileFlagsSetter d v h =
  (setBitWhen d 29 .|. setBitWhen v 30 .|. setBitWhen h 31 .|.)
  where
    setBitWhen cond b =
      if cond
        then bit b
        else 0

diagonalVertialHorizontal :: [(Bool, Bool, Bool)]
diagonalVertialHorizontal =
  (,,) <$> [False, True] <*> [False, True] <*> [False, True]
