{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Dungeon.Item
    ( Item
    , herb
    , name
    , position
    , iconImagePath
    , healAmount
    ) where

import           Control.Lens (makeLenses)
import           Coord        (Coord)
import           Data.Binary  (Binary)
import           Data.Text    (Text)
import           GHC.Generics (Generic)
import           Localization (MultilingualText, multilingualText)

data Item = Item
          { _name          :: MultilingualText
          , _position      :: Coord
          , _iconImagePath :: Text
          , _healAmount    :: Int
          } deriving (Show, Ord, Eq, Generic)
makeLenses ''Item
instance Binary Item

item :: Coord -> Text -> Int -> Item
item p ip h = Item { _name = multilingualText "Herb" "薬草"
                   , _position = p
                   , _iconImagePath = ip
                   , _healAmount = h
                   }

herb :: Coord -> Item
herb p = item p "images/herb.png" 4
