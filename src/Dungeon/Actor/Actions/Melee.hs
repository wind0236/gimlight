{-# LANGUAGE OverloadedStrings #-}

module Dungeon.Actor.Actions.Melee
    ( meleeAction
    ) where

import           Control.Lens          ((^.))
import           Dungeon               (Dungeon, popActorAt, pushActor)
import           Dungeon.Actor         (Actor, getDefence, getHp, getPower,
                                        name, position, receiveDamage)
import           Dungeon.Actor.Actions (Action)
import           Linear.V2             (V2)
import qualified Localization.Texts    as T
import           Log                   (MessageLog, message)

meleeAction :: V2 Int -> Action
meleeAction offset src dungeon =
    result
    where dstPosition = (src ^. position) + offset
          (target, dungeonWithoutTarget) = popActorAt dstPosition dungeon

          result =
            case target of
                Nothing       -> (([], False), pushActor src dungeon)
                Just defender -> attackFromTo src defender dungeonWithoutTarget

attackFromTo :: Actor -> Actor -> Dungeon -> ((MessageLog, Bool), Dungeon)
attackFromTo attacker defender dungeonWithoutTarget =
  let damage = getPower attacker - getDefence defender
  in if damage > 0
        then let newDefender = receiveDamage damage defender
                 messages = if getHp newDefender <= 0
                                then [message $ T.damagedMessage (attacker ^. name) (defender ^. name) damage, message $ T.deathMessage (defender ^. name)]
                                else [message $ T.damagedMessage (attacker ^. name) (defender ^. name) damage]
                 actorHandler = if getHp newDefender > 0
                                  then pushActor attacker . pushActor newDefender
                                  else pushActor attacker
                 dungeonAfterAttack = actorHandler dungeonWithoutTarget
             in ((map message messages, True), dungeonAfterAttack)
          else (([message $ T.noDamageMessage (attacker ^. name) (defender ^. name)], True), pushActor attacker $ pushActor defender dungeonWithoutTarget)
