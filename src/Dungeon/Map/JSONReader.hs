{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Dungeon.Map.JSONReader
    ( readMapTileImage
    ) where

import           Control.Lens                (Ixed (ix), (^..), (^?))
import           Control.Monad               (unless)
import           Control.Monad.Except        (ExceptT (ExceptT), runExceptT)
import           Data.Aeson.Lens             (_Array, _Integer, _String, key,
                                              values)
import           Data.Array                  (array)
import           Data.Either.Combinators     (maybeToRight)
import           Data.Text                   (unpack)
import           Data.Vector                 (Vector, toList)
import qualified Data.Vector                 as V
import           Dungeon.Map.Cell            (CellMap,
                                              TileIdentifierLayer (TileIdentifierLayer),
                                              cellMap)
import           Dungeon.Map.Tile            (TileCollection, TileIdentifier)
import           Dungeon.Map.Tile.JSONReader (addTileFile)
import           Linear.V2                   (V2 (V2))
import           System.Directory            (canonicalizePath,
                                              makeRelativeToCurrentDirectory)
import           System.FilePath             (dropFileName, (</>))

readMapTileImage :: TileCollection -> FilePath -> IO (CellMap, TileCollection)
readMapTileImage tc path =
    result >>= \case
        Right x -> return x
        Left x  -> error x
  where
    result =
        runExceptT $ do
            (cm, tileJsonPath) <- readMapFile path
            tc' <- ExceptT . fmap return $ addTileFile tileJsonPath tc
            return (cm, tc')

readMapFile :: FilePath -> ExceptT String IO (CellMap, FilePath)
readMapFile path = do
    json <- ExceptT . fmap return $ readFile path
    tileFilePath <- getAndCanonicalizeTileFilePath json
    cm <- ExceptT . return $ parseFile json tileFilePath
    return (cm, tileFilePath)
  where
    getAndCanonicalizeTileFilePath json = do
        rawPath <-
            ExceptT . return . maybeToRight tilePathNotContained $
            getTileFilePath json
        ExceptT . fmap return $ canonicalizePath (dropFileName path </> rawPath) >>=
            makeRelativeToCurrentDirectory
    parseFile json canonicalizedPath = do
        V2 width height <- maybeToRight noWidthOrHeight $ getMapSize json
        tiles <- getTiles json canonicalizedPath
        unless (height * width == length tiles) $ Left invalidWidthHeight
        Right
            (cellMap $ array (V2 0 0, V2 (width - 1) (height - 1)) $
             zip [V2 x y | y <- [0 .. height - 1], x <- [0 .. width - 1]] $
             toList tiles)
    tilePathNotContained =
        "The map file " ++ path ++
        " does not contain the paths to the tile files."
    noWidthOrHeight =
        "The map file " ++ path ++
        " does not contain both `width` and `height` fields."
    invalidWidthHeight =
        "The multiplication of width and height of the map " ++ path ++
        " does not equal to the number of tiles."

getTileFilePath :: String -> Maybe FilePath
getTileFilePath json =
    fmap unpack $ json ^? key "tilesets" . values . key "source" . _String

getMapSize :: String -> Maybe (V2 Int)
getMapSize json =
    case (json ^? key "width" . _Integer, json ^? key "height" . _Integer) of
        (Just w, Just h) -> Just $ fromIntegral <$> V2 w h
        _                -> Nothing

getTiles :: String -> FilePath -> Either String (Vector TileIdentifierLayer)
getTiles json path = V.zipWith TileIdentifierLayer <$> uppers <*> lowers
  where
    lowers =
        maybeToRight (missingLayer "lower") $ getTileIdOfNthLayer 0 json path
    uppers =
        maybeToRight (missingLayer "upper") $ getTileIdOfNthLayer 1 json path
    missingLayer which =
        "The map file does not contain the " ++ which ++ " layer."

getTileIdOfNthLayer ::
       Int -> String -> FilePath -> Maybe (Vector (Maybe TileIdentifier))
getTileIdOfNthLayer n json path = fmap rawIdToMaybe <$> getDataOfNthLayer n json
  where
    rawIdToMaybe 0 = Nothing
    rawIdToMaybe x = Just (path, x - 1)

getDataOfNthLayer :: Int -> String -> Maybe (Vector Int)
getDataOfNthLayer n json = getDataOfAllLayer json >>= (^? ix n)

getDataOfAllLayer :: String -> Maybe [Vector Int]
getDataOfAllLayer json =
    mapM
        (mapM (fmap fromInteger . (^? _Integer)))
        (json ^.. key "layers" . values . key "data" . _Array)
