{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Localization
    ( MultilingualText
    , multilingualText
    , getLocalizedText
    ) where
import           Data.Binary  (Binary)
import           Data.Text    (Text, append)
import           GHC.Generics (Generic)
import           Game.Config  (Config, Language (English, Japanese), getLocale)

data MultilingualText = MultilingualText
                      { en :: Text
                      , ja :: Text
                      } deriving (Show, Eq, Ord, Generic)

instance Binary MultilingualText

instance Semigroup MultilingualText where
    (<>) a b = multilingualText (en a `append` en b) (ja a `append` ja b)

instance Monoid MultilingualText where
    mempty = multilingualText "" ""
    mappend = (<>)

multilingualText :: Text -> Text -> MultilingualText
multilingualText = MultilingualText

getLocalizedText :: Config -> MultilingualText -> Text
getLocalizedText cfg lt =
    case getLocale cfg of
        Just English  -> en lt
        Just Japanese -> ja lt
        Nothing       -> error "The language is not set."
