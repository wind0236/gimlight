module UI.Types
    ( AppEvent(..)
    ) where

import           Data.Text  (Text)
import           GameStatus (GameStatus)

data AppEvent = AppInit
              | AppSaveFinished
              | AppLoadFinished GameStatus
              | AppKeyboardInput Text deriving (Eq, Show)
