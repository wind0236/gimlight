{-# LANGUAGE OverloadedStrings #-}

module Dungeon.Map.JSONReader
    ( readMapFile
    ) where

import           Control.Lens     ((^?))
import           Control.Monad    (guard)
import           Data.Aeson.Lens  (_Array, _Integer, key, values)
import           Data.Array       (array)
import           Data.Vector      (toList)
import           Dungeon.Map.Cell (CellMap, cellMap)
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
        zip [V2 x y | y <- [0 .. height - 1], x <- [0 .. width - 1]] tiles

getMapSize :: String -> Maybe (V2 Int)
getMapSize json =
    case (json ^? key "width" . _Integer, json ^? key "height" . _Integer) of
        (Just w, Just h) -> Just $ fromIntegral <$> V2 w h
        _                -> Nothing

getTiles :: String -> Maybe [Maybe TileId]
getTiles json =
    json ^? key "layers" . values . key "data" . _Array >>=
    fmap (map $ rawIdToMaybe . fromIntegral) .
    mapM (^? _Integer) .
    toList
  where
    rawIdToMaybe i
        | i == 0 = Nothing
        | otherwise = Just (i - 1)
