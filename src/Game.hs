-- You should not define any functions in this module and instead just
-- expose `Game`'s internal structure. Otherwise, we will drown in lots of
-- functions.

module Game
    ( Game(..)
    ) where

import           Game.Config (Config)
import           Game.Status (GameStatus)

data Game = Game
          { status :: GameStatus
          , config :: Config
          } deriving (Eq, Show)
