module Action.Move
    ( moveAction
    ) where

import           Action               (Action, ActionResult (ActionResult),
                                       ActionStatus (Failed, Ok))
import           Control.Lens         ((^.))
import           Control.Monad.Writer (tell)
import           Dungeon              (cellMap, pushActor)
import           Dungeon.Map.Cell     (isWalkableAt)
import           Linear.V2            (V2)
import qualified Localization.Texts   as T

moveAction :: V2 Int -> Action
moveAction offset position src tiles d
    | isWalkableAt (position + offset) tiles (d ^. cellMap) =
        return $ ActionResult Ok (pushActor (position + offset) src d) []
    | otherwise = do
        tell [T.youCannotMoveThere]
        return $ ActionResult Failed (pushActor position src d) []
