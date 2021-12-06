module Action.Wait
    ( waitAction
    ) where

import           Action (Action, ActionResult (ActionResult), ActionStatus (Ok))

waitAction :: Action
waitAction _ _ cm = return $ ActionResult Ok cm []
