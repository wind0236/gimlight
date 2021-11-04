{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Dungeon.Item
    ( Item
    , Effect(..)
    , herb
    , sampleBook
    , getName
    , getPosition
    , getIconImagePath
    , getEffect
    , isUsableManyTimes
    ) where

import           Coord              (Coord)
import           Data.Binary        (Binary)
import           Data.Text          (Text)
import           Dungeon.Item.Book  (Book)
import           Dungeon.Item.Heal  (HealHandler, healHandler)
import           GHC.Generics       (Generic)
import           Localization       (MultilingualText)
import qualified Localization.Texts as T

data Effect
    = Heal HealHandler
    | Book Book
    deriving (Show, Ord, Eq, Generic)

instance Binary Effect

data Item =
    Item
        { name            :: MultilingualText
        , position        :: Coord
        , iconImagePath   :: Text
        , effect          :: Effect
        , usableManyTimes :: Bool
        }
    deriving (Show, Ord, Eq, Generic)

instance Binary Item

item :: MultilingualText -> Coord -> Text -> Effect -> Bool -> Item
item n p ip e u =
    Item
        { name = n
        , position = p
        , iconImagePath = ip
        , effect = e
        , usableManyTimes = u
        }

getName :: Item -> MultilingualText
getName Item {name = n} = n

getPosition :: Item -> Coord
getPosition Item {position = p} = p

getIconImagePath :: Item -> Text
getIconImagePath Item {iconImagePath = ip} = ip

getEffect :: Item -> Effect
getEffect Item {effect = e} = e

isUsableManyTimes :: Item -> Bool
isUsableManyTimes Item {usableManyTimes = u} = u

herb :: Coord -> Item
herb p = item T.herb p "images/herb.png" (Heal $ healHandler 4) False

sampleBook :: Coord -> Item
sampleBook p =
    item T.sampleBook p "images/book.png" (Book T.sampleBookContent) True
