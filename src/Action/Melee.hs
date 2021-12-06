module Action.Melee
    ( meleeAction
    ) where

import           Action           (Action, ActionResult (ActionResult),
                                   ActionResultWithLog,
                                   ActionStatus (Failed, Ok))
import           Actor            (Actor)
import qualified Actor            as A
import           Coord            (Coord)
import           Data.Maybe       (fromMaybe)
import           Dungeon.Map.Cell (CellMap, locateActorAt, removeActorAt)
import           Linear.V2        (V2)

meleeAction :: V2 Int -> Action
meleeAction offset srcPosition src _ cm = result
  where
    dstPosition = srcPosition + offset
    result =
        case removeActorAt dstPosition cm of
            Nothing ->
                return $
                ActionResult
                    Failed
                    (fromMaybe
                         (error "Failed to locate an attacker.")
                         (locateActorAt src srcPosition cm))
                    []
            Just (defender, newCellMap) ->
                attackFromTo srcPosition dstPosition src defender newCellMap

attackFromTo ::
       Coord -> Coord -> Actor -> Actor -> CellMap -> ActionResultWithLog
attackFromTo attackerPosition defenderPosition attacker defender cm = do
    (newAttacker, newDefender) <- A.attackFromTo attacker defender
    let (newDungeon, killed) =
            case newDefender of
                Just x ->
                    ( fromMaybe
                          (error "Failed to locate actors.")
                          (locateActorAt newAttacker attackerPosition cm >>=
                           locateActorAt x defenderPosition)
                    , [])
                Nothing ->
                    ( fromMaybe
                          (error "Failed to locate an actor.")
                          (locateActorAt newAttacker attackerPosition cm)
                    , [defender])
    return $ ActionResult Ok newDungeon killed
