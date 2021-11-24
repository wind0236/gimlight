module Dungeon.Generate.Config
    ( Config(..)
    ) where

import           Linear.V2 (V2)

data Config =
    Config
        { numOfFloors :: Int
        , maxRooms    :: Int
        , roomMinSize :: Int
        , roomMaxSize :: Int
        , mapSize     :: V2 Int
        }
