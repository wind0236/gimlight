{-# LANGUAGE OverloadedStrings #-}

module Dungeon.Map.JSONReader
    ( readMapFile
    ) where

import           Control.Lens     ((^..), (^?))
import           Control.Monad    (guard)
import           Data.Aeson.Lens  (_Array, _Integer, key, values)
import           Data.Array       (array)
import           Data.Vector      (Vector, toList)
import qualified Data.Vector      as V
import           Dungeon.Map.Cell (CellMap, TileIdLayer (TileIdLayer), cellMap)
import           Dungeon.Map.Tile (TileId)
import           Linear.V2        (V2 (V2))

readMapFile :: FilePath -> IO (Maybe CellMap)
readMapFile path = parseMapFile <$> readFile path

parseMapFile :: String -> Maybe CellMap
parseMapFile json = do
    V2 height width <- getMapSize json
    tiles <- getTiles json
    guard $ height * width == length tiles
    Just $ cellMap $ array (V2 0 0, V2 (width - 1) (height - 1)) $
        zip [V2 x y | y <- [0 .. height - 1], x <- [0 .. width - 1]] $
        toList tiles

getMapSize :: String -> Maybe (V2 Int)
getMapSize json =
    case (json ^? key "width" . _Integer, json ^? key "height" . _Integer) of
        (Just w, Just h) -> Just $ fromIntegral <$> V2 w h
        _                -> Nothing

getTiles :: String -> Maybe (Vector TileIdLayer)
getTiles json = V.zipWith TileIdLayer <$> uppers <*> lowers
  where
    lowers = getTileIdOfNthLayer 0 json
    uppers = getTileIdOfNthLayer 1 json

getTileIdOfNthLayer :: Int -> String -> Maybe (Vector (Maybe TileId))
getTileIdOfNthLayer n json = fmap rawIdToMaybe <$> getDataOfNthLayer n json
  where
    rawIdToMaybe 0 = Nothing
    rawIdToMaybe x = Just $ x - 1

getDataOfNthLayer :: Int -> String -> Maybe (Vector Int)
getDataOfNthLayer n json =
    case getDataOfAllLayer json of
        Just x ->
            if length x > n
                then Just $ x !! n
                else Nothing
        Nothing -> Nothing

getDataOfAllLayer :: String -> Maybe [Vector Int]
getDataOfAllLayer json =
    mapM
        (mapM (fmap fromInteger . (^? _Integer)))
        (json ^.. key "layers" . values . key "data" . _Array)
