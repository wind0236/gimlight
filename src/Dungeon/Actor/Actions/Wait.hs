module Dungeon.Actor.Actions.Wait
    ( waitAction
    ) where

import           Dungeon               (pushActor)
import           Dungeon.Actor.Actions (Action)

waitAction :: Action
waitAction e d = return $ pushActor e d
