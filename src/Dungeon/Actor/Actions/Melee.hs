{-# LANGUAGE OverloadedStrings #-}

module Dungeon.Actor.Actions.Melee
    ( meleeAction
    ) where

import           Control.Lens          ((^.))
import           Data.Text             (append, pack)
import           Dungeon               (popActorAt, pushActor)
import           Dungeon.Actor         (defence, getHp, name, position, power,
                                        updateHp)
import           Dungeon.Actor.Actions (Action)
import           Linear.V2             (V2)
import           Localization          (multilingualText)
import           Log                   (message)

meleeAction :: V2 Int -> Action
meleeAction offset src dungeon =
    result
    where dstPosition = (src ^. position) + offset
          (target, dungeonWithoutTarget) = popActorAt dstPosition dungeon

          result =
            case target of
                Nothing       -> (([], False), pushActor src dungeon)
                Just defender -> attackFromTo src defender

          attackFromTo attacker defender =
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
                    else (([message $ noDamage attacker defender], True), pushActor attacker $ pushActor defender dungeonWithoutTarget)


          attackMessage from to =
            mconcat [ from ^. name
                    , multilingualText " attacks" "は"
                    , to ^. name
                    , multilingualText "" "に攻撃"
                    ]

          damagedMessage from to damage =

              attackMessage from to <>
                multilingualText (" for " `append` pack (show damage) `append` " hit points.")
                                 ("して" `append` pack (show damage) `append` "ポイントのダメージを与えた．")
          deathMessage who = (who ^. name) <> multilingualText " is dead!" "は死んだ．"
          noDamage from to = attackMessage from to `mappend`
                                multilingualText " but does not damage." "したがダメージを受けなかった．"
