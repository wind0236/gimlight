module Action.Move
    ( moveAction
    ) where

import           Action               (Action, ActionResult (ActionResult),
                                       ActionStatus (Failed, Ok))
import           Control.Monad.Writer (tell)
import           Data.Maybe           (fromMaybe)
import           Dungeon.Map.Cell     (isWalkableAt, locateActorAt)
import           Linear.V2            (V2)
import qualified Localization.Texts   as T

moveAction :: V2 Int -> Action
moveAction offset position src tiles cm
    | isWalkableAt (position + offset) tiles cm =
        return $
        ActionResult
            Ok
            (fromMaybe
                 (error "Failed to locate an actor.")
                 (locateActorAt src (position + offset) cm))
            []
    | otherwise = do
        tell [T.youCannotMoveThere]
        return $
            ActionResult
                Failed
                (fromMaybe
                     (error "Failed to locate an actor.")
                     (locateActorAt src position cm))
                []
