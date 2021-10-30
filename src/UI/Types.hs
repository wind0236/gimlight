module UI.Types
    ( AppEvent(..)
    , GameWidgetEnv
    , GameWidgetNode
    ) where

import           Data.Text (Text)
import           GameModel (GameModel)
import           Monomer   (WidgetEnv, WidgetNode)

data AppEvent = AppInit
              | AppSaveFinished
              | AppLoadFinished GameModel
              | AppKeyboardInput Text deriving (Eq, Show)

type GameWidgetEnv = WidgetEnv GameModel AppEvent
type GameWidgetNode = WidgetNode GameModel AppEvent
