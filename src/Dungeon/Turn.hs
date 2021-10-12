module Dungeon.Turn
    ( Status(..)
    ) where

data Status = Success | PlayerKilled deriving (Show, Ord, Eq)
