{-# LANGUAGE DeriveGeneric #-}

module Dungeon.Identifier
    ( Identifier(..)
    , isTown
    ) where

import           Data.Binary  (Binary)
import           GHC.Generics (Generic)

data Identifier
    = Beaeve
    | BatsCave
    | GlobalMap
    deriving (Show, Ord, Eq, Generic)

instance Binary Identifier

isTown :: Identifier -> Bool
isTown Beaeve = True
isTown _      = False
