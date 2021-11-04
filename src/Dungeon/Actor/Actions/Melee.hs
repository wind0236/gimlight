{-# LANGUAGE OverloadedStrings #-}

module Dungeon.Actor.Actions.Melee
    ( meleeAction
    ) where

import           Control.Lens          ((^.))
import           Control.Monad.Writer  (Writer)
import           Dungeon               (Dungeon, popActorAt, pushActor)
import           Dungeon.Actor         (Actor, position)
import qualified Dungeon.Actor         as A
import           Dungeon.Actor.Actions (Action, ActionStatus (Failed, Ok))
import           Linear.V2             (V2)
import           Log                   (MessageLog)

meleeAction :: V2 Int -> Action
meleeAction offset src dungeon =
    result
    where dstPosition = (src ^. position) + offset
          (target, dungeonWithoutTarget) = popActorAt dstPosition dungeon

          result =
            case target of
                Nothing       -> return (Failed, pushActor src dungeon)
                Just defender -> attackFromTo src defender dungeonWithoutTarget

attackFromTo :: Actor -> Actor -> Dungeon -> Writer MessageLog (ActionStatus, Dungeon)
attackFromTo attacker defender dungeonWithoutAttackerAndDefender = do
    (newAttacker, newDefender) <- A.attackFromTo attacker defender

    return (Ok, pushActor newAttacker $ maybe dungeonWithoutAttackerAndDefender (`pushActor` dungeonWithoutAttackerAndDefender) newDefender)
