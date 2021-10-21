module UI.Types
    ( AppEvent(..)
    ) where

import           Data.Text (Text)
import           Game      (Game)

data AppEvent = AppInit
              | AppSaveFinished
              | AppLoadFinished Game
              | AppKeyboardInput Text deriving (Eq, Show)
