module UI.Types
    ( Tick(..)
    , Name
    , AppEvent(..)
    ) where

import           Engine  (Engine)
import           Monomer (KeyCode)

data Tick = Tick

type Name = ()

data AppEvent = AppInit
              | AppSaveFinished
              | AppLoadFinished Engine
              | AppKeyboardInput KeyCode deriving (Eq, Show)
