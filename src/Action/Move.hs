module Action.Move
    ( moveAction
    ) where

import           Action               (Action, ActionResult (ActionResult),
                                       ActionStatus (Failed, Ok))
import           Control.Lens         (Ixed (ix), (^?))
import           Control.Monad.Writer (tell)
import           Data.Maybe           (fromMaybe)
import           Dungeon.Map.Cell     (isWalkable, locateActorAt, removeActorAt)
import           Linear.V2            (V2)
import qualified Localization.Texts   as T

moveAction :: V2 Int -> Action
moveAction offset position tiles cm
    | maybe False (isWalkable tiles) $ cm ^? ix (position + offset) =
        case removeActorAt position cm of
            Just (src, ncm) ->
                return $
                ActionResult
                    Ok
                    (fromMaybe
                         (error "Failed to locate an actor.")
                         (locateActorAt src (position + offset) ncm))
                    []
            Nothing -> return $ ActionResult Failed cm []
    | otherwise = do
        tell [T.youCannotMoveThere]
        return $ ActionResult Failed cm []
