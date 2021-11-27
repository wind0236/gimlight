module Action.Melee
    ( meleeAction
    ) where

import           Action    (Action, ActionResult (ActionResult),
                            ActionResultWithLog, ActionStatus (Failed, Ok))
import           Actor     (Actor)
import qualified Actor     as A
import           Coord     (Coord)
import           Dungeon   (Dungeon, popActorAt, pushActor)
import           Linear.V2 (V2)

meleeAction :: V2 Int -> Action
meleeAction offset srcPosition src _ dungeon = result
  where
    dstPosition = srcPosition + offset
    (target, dungeonWithoutTarget) = popActorAt dstPosition dungeon
    result =
        case target of
            Nothing ->
                return $
                ActionResult Failed (pushActor srcPosition src dungeon) []
            Just defender ->
                attackFromTo
                    srcPosition
                    dstPosition
                    src
                    defender
                    dungeonWithoutTarget

attackFromTo ::
       Coord -> Coord -> Actor -> Actor -> Dungeon -> ActionResultWithLog
attackFromTo attackerPosition defenderPosition attacker defender d = do
    (newAttacker, newDefender) <- A.attackFromTo attacker defender
    let (newDungeon, killed) =
            case newDefender of
                Just x ->
                    ( pushActor attackerPosition newAttacker $
                      pushActor defenderPosition x d
                    , [])
                Nothing ->
                    (pushActor attackerPosition newAttacker d, [defender])
    return $ ActionResult Ok newDungeon killed
