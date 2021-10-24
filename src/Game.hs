module Game
    ( Game(..)
    ) where

import           Game.Config (Config)
import           Game.Status (GameStatus)

data Game = Game
          { status :: GameStatus
          , config :: Config
          } deriving (Eq, Show)
