module Action.Wait
    ( waitAction
    ) where

import           Action           (Action, ActionResult (ActionResult),
                                   ActionStatus (Ok))
import           Data.Maybe       (fromMaybe)
import           Dungeon.Map.Cell (locateActorAt)

waitAction :: Action
waitAction position e _ cm = return $ ActionResult Ok newCm []
  where
    newCm =
        fromMaybe
            (error "Failed to locate an actor.")
            (locateActorAt e position cm)
