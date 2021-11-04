module Dungeon.Size
    ( minSize
    , maxSize
    ) where

-- Must not be less than `max tileColumns tileWidth`
--
-- TODO: Calculate it.
minSize, maxSize :: Int
minSize = 25

maxSize = 100
