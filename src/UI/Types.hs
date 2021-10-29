module UI.Types
    ( AppEvent(..)
    , GameWidgetEnv
    , GameWidgetNode
    ) where

import           Data.Text (Text)
import           Game      (Game)
import           Monomer   (WidgetEnv, WidgetNode)

data AppEvent = AppInit
              | AppSaveFinished
              | AppLoadFinished Game
              | AppKeyboardInput Text deriving (Eq, Show)

type GameWidgetEnv = WidgetEnv Game AppEvent
type GameWidgetNode = WidgetNode Game AppEvent
