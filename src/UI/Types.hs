module UI.Types
    ( Tick(..)
    , Name
    , AppEvent(..)
    ) where

import           Data.Text (Text)
import           Engine    (Engine)

data Tick = Tick

type Name = ()

data AppEvent = AppInit
              | AppSaveFinished
              | AppLoadFinished Engine
              | AppKeyboardInput Text deriving (Eq, Show)
