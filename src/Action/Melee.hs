{-# LANGUAGE TupleSections #-}

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
meleeAction offset srcPosition _ cm = result
  where
    dstPosition = srcPosition + offset
    result =
        case removeResult of
            Nothing -> return $ ActionResult Failed cm []
            Just (attacker, (defender, newCellMap)) ->
                attackFromTo
                    srcPosition
                    dstPosition
                    attacker
                    defender
                    newCellMap
    removeResult =
        removeActorAt srcPosition cm >>=
        (\(a, cm') -> (a, ) <$> removeActorAt dstPosition cm')

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
