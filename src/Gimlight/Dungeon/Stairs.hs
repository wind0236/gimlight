{-# LANGUAGE DeriveGeneric #-}

module Gimlight.Dungeon.Stairs
    ( StairsPair(..)
    ) where

import           Data.Binary    (Binary)
import           GHC.Generics   (Generic)
import           Gimlight.Coord (Coord)

data StairsPair =
    StairsPair
        { upStairs   :: Coord
        , downStairs :: Coord
        }
    deriving (Show, Ord, Eq, Generic)

instance Binary StairsPair
