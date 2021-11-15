module Action.Wait
    ( waitAction
    ) where

import           Action  (Action, ActionResult (ActionResult),
                          ActionStatus (Ok))
import           Dungeon (pushActor)

waitAction :: Action
waitAction e d = return $ ActionResult Ok (pushActor e d) []
