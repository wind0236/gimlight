{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Gimlight.GameConfig
  ( GameConfig,
    Language (..),
    readConfigOrDefault,
    writeConfig,
    setLocale,
    getLocale,
  )
where

import Data.Binary (Binary, decodeFile, encodeFile)
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import System.Directory (doesFileExist)

data Language
  = English
  | Japanese
  deriving (Eq, Show, Generic)

instance Binary Language

newtype GameConfig = GameConfig
  { language :: Maybe Language
  }
  deriving (Eq, Show, Generic)

instance Binary GameConfig

readConfigOrDefault :: IO GameConfig
readConfigOrDefault = fromMaybe initConfig <$> tryReadConfig

tryReadConfig :: IO (Maybe GameConfig)
tryReadConfig = do
  fileExists <- doesFileExist configFilePath
  if fileExists
    then do
      cfg <- decodeFile configFilePath
      return $ Just cfg
    else do
      encodeFile configFilePath initConfig
      return Nothing

writeConfig :: GameConfig -> IO ()
writeConfig = encodeFile configFilePath

initConfig :: GameConfig
initConfig = GameConfig {language = Nothing}

setLocale :: Language -> GameConfig -> GameConfig
setLocale l c = c {language = Just l}

getLocale :: GameConfig -> Maybe Language
getLocale GameConfig {language = l} = l

configFilePath :: FilePath
configFilePath = "config"
