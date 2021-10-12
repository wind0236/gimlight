module Dungeon.Size
    ( maxRooms
    , roomMinSize
    , roomMaxSize
    , minSize
    , maxSize
    ) where

maxRooms, roomMinSize, roomMaxSize, minSize, maxSize :: Int
maxRooms = 30
roomMinSize = 6
roomMaxSize = 10

-- Must not be less than `max tileColumns tileWidth`
--
-- TODO: Calculate it.
minSize = 20
maxSize = 100
