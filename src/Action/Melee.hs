module Action.Melee
    ( meleeAction
    ) where

import           Action       (Action, ActionResult (ActionResult),
                               ActionResultWithLog, ActionStatus (Failed, Ok))
import           Actor        (Actor, position)
import qualified Actor        as A
import           Control.Lens ((^.))
import           Dungeon      (Dungeon, popActorAt, pushActor)
import           Linear.V2    (V2)

meleeAction :: V2 Int -> Action
meleeAction offset src dungeon = result
  where
    dstPosition = src ^. position + offset
    (target, dungeonWithoutTarget) = popActorAt dstPosition dungeon
    result =
        case target of
            Nothing -> return $ ActionResult Failed (pushActor src dungeon) []
            Just defender -> attackFromTo src defender dungeonWithoutTarget

attackFromTo :: Actor -> Actor -> Dungeon -> ActionResultWithLog
attackFromTo attacker defender d = do
    (newAttacker, newDefender) <- A.attackFromTo attacker defender
    let (newDungeon, killed) =
            case newDefender of
                Just x  -> (pushActor newAttacker $ pushActor x d, [])
                Nothing -> (pushActor newAttacker d, [defender])
    return $ ActionResult Ok newDungeon killed
