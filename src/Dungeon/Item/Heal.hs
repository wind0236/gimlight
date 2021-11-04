{-# LANGUAGE DeriveGeneric #-}

module Dungeon.Item.Heal
    ( HealHandler
    , healHandler
    , getHealAmount
    ) where

import           Data.Binary  (Binary)
import           GHC.Generics (Generic)

newtype HealHandler =
    HealHandler
        { amount :: Int
        }
    deriving (Show, Ord, Eq, Generic)

instance Binary HealHandler

healHandler :: Int -> HealHandler
healHandler = HealHandler

getHealAmount :: HealHandler -> Int
getHealAmount (HealHandler a) = a
