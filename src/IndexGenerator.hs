module IndexGenerator
    ( IndexGenerator
    , Index
    , generator
    , generate
    ) where

type IndexGenerator = Int

type Index = Int

generator :: IndexGenerator
generator = 0

-- I assume that the number of actors in the game that exist in the same
-- time will not exceed 0xffff_ffff.
generate :: IndexGenerator -> (Index, IndexGenerator)
generate n = (n, n + 1)
