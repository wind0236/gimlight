{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Dungeon.Item
    ( Item
    , herb
    , getName
    , getPosition
    , getIconImagePath
    , getHealAmount
    ) where

import           Coord              (Coord)
import           Data.Binary        (Binary)
import           Data.Text          (Text)
import           GHC.Generics       (Generic)
import           Localization       (MultilingualText)
import qualified Localization.Texts as T

data Item = Item
          { name          :: MultilingualText
          , position      :: Coord
          , iconImagePath :: Text
          , healAmount    :: Int
          } deriving (Show, Ord, Eq, Generic)

instance Binary Item

item :: Coord -> Text -> Int -> Item
item p ip h = Item { name = T.herb
                   , position = p
                   , iconImagePath = ip
                   , healAmount = h
                   }

getName :: Item -> MultilingualText
getName Item { name = n } = n

getPosition :: Item -> Coord
getPosition Item { position = p } = p

getIconImagePath :: Item -> Text
getIconImagePath Item { iconImagePath = ip } = ip

getHealAmount :: Item -> Int
getHealAmount Item { healAmount = h } = h

herb :: Coord -> Item
herb p = item p "images/herb.png" 4
