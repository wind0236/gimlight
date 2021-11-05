module Actor.Actions.Wait
    ( waitAction
    ) where

import           Actor.Actions (Action, ActionStatus (Ok))
import           Dungeon       (pushActor)

waitAction :: Action
waitAction e d = return (Ok, pushActor e d)
