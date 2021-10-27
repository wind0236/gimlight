{-# LANGUAGE OverloadedStrings #-}

module Dungeon.Actor.Actions.Melee
    ( meleeAction
    ) where

import           Control.Lens          ((^.))
import           Dungeon               (Dungeon, popActorAt, pushActor)
import           Dungeon.Actor         (Actor, defence, getHp, name, position,
                                        power, updateHp)
import           Dungeon.Actor.Actions (Action)
import           Linear.V2             (V2)
import           Localization          (multilingualText)
import           Log                   (Message, MessageLog, message)
import           TextShow              (TextShow (showt))

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
  let damage = attacker ^. power - defender ^. defence
  in if damage > 0
        then let newHp = getHp defender - damage
                 newDefender = updateHp defender newHp
                 messages = if newHp <= 0
                                then [damagedMessage attacker defender damage, deathMessage defender]
                                else [damagedMessage attacker defender damage]
                 actorHandler = if newHp > 0
                                  then pushActor attacker . pushActor newDefender
                                  else pushActor attacker
                 dungeonAfterAttack = actorHandler dungeonWithoutTarget
             in ((map message messages, True), dungeonAfterAttack)
          else (([message $ noDamageMessage attacker defender], True), pushActor attacker $ pushActor defender dungeonWithoutTarget)

damagedMessage :: Actor -> Actor -> Int -> Message
damagedMessage from to damage =
    attackMessage from to <>
        multilingualText (" for " <> showt damage <> " hit points.")
                         ("して" <> showt damage <> "ポイントのダメージを与えた．")

deathMessage :: Actor -> Message
deathMessage who = (who ^. name) <> multilingualText " is dead!" "は死んだ．"

attackMessage :: Actor -> Actor -> Message
attackMessage from to =
    mconcat [ from ^. name
            , multilingualText " attacks " "は"
            , to ^. name
            , multilingualText "" "に攻撃"
            ]

noDamageMessage :: Actor -> Actor -> Message
noDamageMessage from to = attackMessage from to `mappend`
                    multilingualText " but does not damage." "したがダメージを受けなかった．"
