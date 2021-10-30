-- You should not define any functions in this module and instead just
-- expose `GameModel`'s internal structure. Otherwise, we will drown in lots of
-- functions.

module GameModel
    ( GameModel(..)
    ) where

import           GameModel.Config (Config)
import           GameModel.Status (GameStatus)

data GameModel = GameModel
               { status :: GameStatus
               , config :: Config
               } deriving (Eq, Show)
