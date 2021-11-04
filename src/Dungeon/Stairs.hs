{-# LANGUAGE DeriveGeneric #-}

module Dungeon.Stairs
    ( StairsPair(..)
    ) where

import           Coord        (Coord)
import           Data.Binary  (Binary)
import           GHC.Generics (Generic)

data StairsPair =
    StairsPair
        { upStairs   :: Coord
        , downStairs :: Coord
        }
    deriving (Show, Ord, Eq, Generic)

instance Binary StairsPair
