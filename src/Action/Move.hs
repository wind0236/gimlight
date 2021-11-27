module Action.Move
    ( moveAction
    ) where

import           Action               (Action, ActionResult (ActionResult),
                                       ActionStatus (Failed, Ok))
import           Control.Lens         ((^.))
import           Control.Monad.Writer (tell)
import           Coord                (Coord)
import           Dungeon              (Dungeon, cellMap, pushActor)
import           Dungeon.Map.Cell     (isWalkableAt)
import           Dungeon.Map.Tile     (TileCollection)
import           Linear.V2            (V2)
import qualified Localization.Texts   as T

moveAction :: V2 Int -> Action
moveAction offset position src tiles d
    | not (movable tiles d (position + offset)) = do
        tell [T.youCannotMoveThere]
        return $ ActionResult Failed (pushActor position src d) []
    | otherwise =
        return $ ActionResult Ok (pushActor (position + offset) src d) []

movable :: TileCollection -> Dungeon -> Coord -> Bool
movable ts d c = isWalkableAt c ts (d ^. cellMap)
