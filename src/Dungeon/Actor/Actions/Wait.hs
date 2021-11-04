module Dungeon.Actor.Actions.Wait
    ( waitAction
    ) where

import           Dungeon               (pushActor)
import           Dungeon.Actor.Actions (Action, ActionStatus (Ok))

waitAction :: Action
waitAction e d = return (Ok, pushActor e d)
