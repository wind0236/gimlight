-- You should not define any functions in this module and instead just
-- expose `GameModel`'s internal structure. Otherwise, we will drown in lots of
-- functions.
module GameModel
    ( GameModel(..)
    ) where

import           GameConfig (GameConfig)
import           GameStatus (GameStatus)

data GameModel =
    GameModel
        { status :: GameStatus
        , config :: GameConfig
        }
    deriving (Eq, Show)
