module IndexGenerator
    ( IndexGenerator
    , Index
    , generator
    , generate
    ) where

import           Control.Monad.State (MonadState (state), State)

type IndexGenerator = Int

type Index = Int

generator :: IndexGenerator
generator = 0

-- I assume that the number of actors in the game that exist in the same
-- time will not exceed 0xffff_ffff.
generate :: State IndexGenerator Index
generate = state $ \n -> (n, n + 1)
