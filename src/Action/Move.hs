module Action.Move
    ( moveAction
    ) where

import           Action               (Action, ActionResult (ActionResult),
                                       ActionStatus (Failed, Ok))
import           Control.Monad.State  (execStateT)
import           Control.Monad.Writer (tell)
import           Dungeon.Map.Cell     (Error (ActorAlreadyExists, TileIsNotWalkable),
                                       locateActorAt, removeActorAt)
import           Linear.V2            (V2)
import qualified Localization.Texts   as T

moveAction :: V2 Int -> Action
moveAction offset position tiles cm =
    case result of
        Right x                     -> return $ ActionResult Ok x []
        Left (ActorAlreadyExists _) -> cannotMove
        Left TileIsNotWalkable      -> cannotMove
        _                           -> error "Unreachable."
  where
    result =
        flip execStateT cm $ do
            a <- removeActorAt position
            locateActorAt tiles a (position + offset)
    cannotMove = do
        tell [T.youCannotMoveThere]
        return $ ActionResult Failed cm []
