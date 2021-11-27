module Action.Wait
    ( waitAction
    ) where

import           Action  (Action, ActionResult (ActionResult),
                          ActionStatus (Ok))
import           Dungeon (pushActor)

waitAction :: Action
waitAction position e _ d = return $ ActionResult Ok (pushActor position e d) []
