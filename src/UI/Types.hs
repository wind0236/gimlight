module UI.Types
    ( Tick(..)
    , Name
    , AppEvent(..)
    ) where

import           Data.Text (Text)
import           GameStatus    (GameStatus)

data Tick = Tick

type Name = ()

data AppEvent = AppInit
              | AppSaveFinished
              | AppLoadFinished GameStatus
              | AppKeyboardInput Text deriving (Eq, Show)
