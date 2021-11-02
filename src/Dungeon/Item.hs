{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Dungeon.Item
    ( Item
    , Effect(..)
    , herb
    , getName
    , getPosition
    , getIconImagePath
    , getEffect
    ) where

import           Coord              (Coord)
import           Data.Binary        (Binary)
import           Data.Text          (Text)
import           Dungeon.Item.Heal  (HealHandler, healHandler)
import           GHC.Generics       (Generic)
import           Localization       (MultilingualText)
import qualified Localization.Texts as T

newtype Effect = Heal HealHandler deriving (Show, Ord, Eq, Generic)

instance Binary Effect

data Item = Item
          { name          :: MultilingualText
          , position      :: Coord
          , iconImagePath :: Text
          , effect        :: Effect
          } deriving (Show, Ord, Eq, Generic)

instance Binary Item

item :: Coord -> Text -> Effect -> Item
item p ip e = Item { name = T.herb
                   , position = p
                   , iconImagePath = ip
                   , effect = e
                   }

getName :: Item -> MultilingualText
getName Item { name = n } = n

getPosition :: Item -> Coord
getPosition Item { position = p } = p

getIconImagePath :: Item -> Text
getIconImagePath Item { iconImagePath = ip } = ip

getEffect :: Item -> Effect
getEffect Item { effect = e } = e

herb :: Coord -> Item
herb p = item p "images/herb.png" (Heal $ healHandler 4)
