{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Dungeon.Actor.Inventory
    ( Inventory
    , inventory
    , addItem
    ) where

import           Control.Lens (makeLenses, (%~), (&), (^.))
import           Data.Binary  (Binary)
import           Dungeon.Item (Item)
import           GHC.Generics (Generic)

data Inventory = Inventory
               { _items    :: [Item]
               , _maxItems :: Int
               } deriving (Show, Ord, Eq, Generic)
makeLenses ''Inventory
instance Binary Inventory

inventory :: Int -> Inventory
inventory n = Inventory { _items = []
                        , _maxItems = n
                        }

addItem :: Item -> Inventory -> Maybe Inventory
addItem item inv = if length (inv ^. items) == inv ^. maxItems
                       then Nothing
                       else Just $ inv & items %~ (:) item
