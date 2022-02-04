{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Gimlight.Dungeon.Map.JSONReader
    ( readMapFile
    ) where

import           Control.Lens              (Ixed (ix), (^..), (^?))
import           Control.Monad             (unless)
import           Control.Monad.Except      (ExceptT (ExceptT), runExceptT)
import           Control.Monad.Trans.Maybe (MaybeT (MaybeT), maybeToExceptT)
import           Data.Aeson.Lens           (_Array, _Integer, _String, key,
                                            values)
import           Data.Array                (array)
import           Data.Bifunctor            (Bifunctor (second))
import           Data.Bits                 (Bits (clearBit))
import           Data.Either.Combinators   (maybeToRight)
import           Data.List                 (find, sortBy)
import           Data.Text                 (unpack)
import           Data.Vector               (Vector, toList)
import qualified Data.Vector               as V
import           Gimlight.Data.Either      (expectRight)
import           Gimlight.Data.Maybe       (expectJust)
import           Gimlight.Dungeon.Map.Cell (CellMap,
                                            TileIdLayer (TileIdLayer),
                                            cellMap)
import           Gimlight.Dungeon.Map.Tile (TileId)
import           Linear.V2                 (V2 (V2))
import           System.Directory          (canonicalizePath,
                                            makeRelativeToCurrentDirectory)
import           System.FilePath           (dropFileName, (</>))

readMapFile :: FilePath -> IO CellMap
readMapFile p = fmap (expectRight msg) $ runExceptT $ readMapFileOrFail p
  where
    msg = "Failed to load a map: " ++ p

readMapFileOrFail :: FilePath -> ExceptT String IO CellMap
readMapFileOrFail path = do
    json <- ExceptT . fmap return $ readFile path
    getTiles json path >>= ExceptT . return . parseFile json
  where
    parseFile json tiles = do
        V2 width height <- maybeToRight noWidthOrHeight $ getMapSize json
        unless (height * width == length tiles) $ Left invalidWidthHeight
        Right
            (cellMap $ array (V2 0 0, V2 (width - 1) (height - 1)) $
             zip [V2 x y | y <- [0 .. height - 1], x <- [0 .. width - 1]] $
             toList tiles)
    noWidthOrHeight =
        "The map file " ++ path ++
        " does not contain both `width` and `height` fields."
    invalidWidthHeight =
        "The multiplication of width and height of the map " ++ path ++
        " does not equal to the number of tiles."

getMapSize :: String -> Maybe (V2 Int)
getMapSize json =
    case (fetch "width", fetch "height") of
        (Just w, Just h) -> Just $ fromIntegral <$> V2 w h
        _                -> Nothing
  where
    fetch k = json ^? key k . _Integer

getTiles :: String -> FilePath -> ExceptT String IO (Vector TileIdLayer)
getTiles json pathToMap = V.zipWith TileIdLayer <$> uppers <*> lowers
  where
    lowers = getTileIdOfNthLayerOrErr 0
    uppers = getTileIdOfNthLayerOrErr 1
    getTileIdOfNthLayerOrErr n =
        maybeToExceptT (missingLayer $ show n) $
        getTileIdOfNthLayer n json pathToMap
    missingLayer which =
        "The map file does not contain the level " ++ which ++ " layer."

getTileIdOfNthLayer ::
       Int -> String -> FilePath -> MaybeT IO (Vector (Maybe TileId))
getTileIdOfNthLayer n json pathToMap =
    MaybeT . traverse (mapM rawIdToIdentifier) $ getDataOfNthLayer n json
  where
    rawIdToIdentifier 0 = return Nothing
    rawIdToIdentifier ident =
        (fmap Just . (\(x, y) -> (, y) <$> canonicalizeIdentifier x)) .
        second (ident -) $
        expectJust
            ("Invalid tile GID: " ++ show ident)
            (find ((clearAllFlags ident >=) . snd) $ getSourceAndFirstGid json)
    canonicalizeIdentifier path =
        canonicalizePath (dropFileName pathToMap </> path) >>=
        makeRelativeToCurrentDirectory
    clearAllFlags = (`clearBit` 29) . (`clearBit` 30) . (`clearBit` 31)

getSourceAndFirstGid :: String -> [(FilePath, Int)]
getSourceAndFirstGid json =
    sortBy (\(_, a) (_, b) -> compare b a) $ zip sources firstGids
  where
    sources =
        fmap unpack $ json ^.. key "tilesets" . values . key "source" . _String
    firstGids =
        fmap fromIntegral $ json ^.. key "tilesets" . values . key "firstgid" .
        _Integer

getDataOfNthLayer :: Int -> String -> Maybe (Vector Int)
getDataOfNthLayer n json = getDataOfAllLayer json >>= (^? ix n)

getDataOfAllLayer :: String -> Maybe [Vector Int]
getDataOfAllLayer json =
    mapM
        (mapM (fmap fromInteger . (^? _Integer)))
        (json ^.. key "layers" . values . key "data" . _Array)
