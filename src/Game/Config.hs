{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Game.Config
    ( Config
    , Language(..)
    , readConfigOrDefault
    , writeConfig
    , setLocale
    , getLocale
    ) where

import           Data.Binary      (Binary, decodeFile, encodeFile)
import           Data.Maybe       (fromMaybe)
import           GHC.Generics     (Generic)
import           System.Directory (doesFileExist)

data Language = English | Japanese deriving (Eq, Show, Generic)
instance Binary Language

newtype Config = Config { language :: Maybe Language } deriving (Eq, Show, Generic)
instance Binary Config

readConfigOrDefault :: IO Config
readConfigOrDefault = fromMaybe initConfig <$> tryReadConfig

tryReadConfig :: IO (Maybe Config)
tryReadConfig = do
    fileExists <- doesFileExist configFilePath

    if fileExists
        then do
            cfg <- decodeFile configFilePath
            return $ Just cfg
        else do
            encodeFile configFilePath initConfig
            return Nothing

writeConfig :: Config -> IO ()
writeConfig = encodeFile configFilePath

initConfig :: Config
initConfig = Config { language = Nothing }

setLocale :: Language -> Config -> Config
setLocale l c = c { language = Just l }

getLocale :: Config -> Maybe Language
getLocale Config { language = l } = l

configFilePath :: FilePath
configFilePath = "config"
