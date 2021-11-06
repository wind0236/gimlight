module Action.Wait
    ( waitAction
    ) where

import           Action  (Action, ActionStatus (Ok))
import           Dungeon (pushActor)

waitAction :: Action
waitAction e d = return (Ok, pushActor e d)
