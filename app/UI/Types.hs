module UI.Types
    ( Tick(..)
    , Name
    , AppEvent(..)
    ) where

import           Monomer (KeyCode)

data Tick = Tick

type Name = ()

data AppEvent = AppInit | AppKeyboardInput KeyCode deriving (Eq, Show)
