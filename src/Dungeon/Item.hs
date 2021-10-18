{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Dungeon.Item
    ( Item
    , herb
    , name
    , position
    , iconImagePath
    ) where

import           Control.Lens (makeLenses)
import           Coord        (Coord)
import           Data.Binary  (Binary)
import           Data.Text    (Text)
import           GHC.Generics (Generic)

data Item = Item
          { _name          :: Text
          , _position      :: Coord
          , _iconImagePath :: Text
          } deriving (Show, Ord, Eq, Generic)
makeLenses ''Item
instance Binary Item

item :: Coord -> Text -> Item
item p ip = Item { _name = "Herb"
                 , _position = p
                 , _iconImagePath = ip
                 }

herb :: Coord -> Item
herb p = item p "images/herb.png"
