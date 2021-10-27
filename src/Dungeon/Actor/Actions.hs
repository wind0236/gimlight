{-# LANGUAGE OverloadedStrings #-}

module Dungeon.Actor.Actions
    ( meleeAction
    , moveAction
    , waitAction
    , pickUpAction
    , consumeAction
    , Action
    ) where

import           Control.Lens            ((&), (.~), (^.))
import           Coord                   (Coord)
import           Data.Array              ((!))
import           Data.Maybe              (isNothing)
import           Data.Text               (append, pack)
import           Dungeon                 (Dungeon, actorAt, mapWidthAndHeight,
                                          popActorAt, popItemAt, pushActor,
                                          pushItem, tileMap)
import           Dungeon.Actor           (Actor, defence, getHp, healHp,
                                          inventoryItems, name, position, power,
                                          removeNthItem, updateHp)
import           Dungeon.Actor.Inventory (addItem)
import           Dungeon.Item            (healAmount)
import qualified Dungeon.Item            as I
import           Dungeon.Map.Tile        (walkable)
import           Linear.V2               (V2 (V2))
import           Localization            (multilingualText)
import           Log                     (MessageLog, message)

type Action = Actor -> Dungeon -> ((MessageLog, Bool), Dungeon)

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

moveAction :: V2 Int -> Action
moveAction offset src d = if not (movable d (src ^. position + offset))
                                then (([multilingualText "That way is blocked." "その方向には進めない．"], False), pushActor src d)
                                else (([], True), pushActor (updatePosition d src offset) d)

waitAction :: Action
waitAction e d = (([], True), pushActor e d)

pickUpAction :: Action
pickUpAction e d =
    case item of
        Just x ->
            case addItem x (e ^. inventoryItems) of
                Just xs ->
                    (
                        ([multilingualText "You got " "アイテムを入手しました：" <> (x ^. I.name)], True),
                        pushActor (e & inventoryItems .~ xs) dungeonAfterPickingUp
                    )
                Nothing ->
                    (([multilingualText "Your bag is full." "バッグは一杯だ．"], False), pushItem x $ pushActor e dungeonAfterPickingUp)
        Nothing -> (([multilingualText "You got nothing." "あなたは無を手に入れた．"], False), pushActor e dungeonAfterPickingUp)
    where (item, dungeonAfterPickingUp) = popItemAt (e ^. position) d

consumeAction :: Int -> Action
consumeAction n e d =
    case item of
        Just x ->
            (
                ([(e ^. name)
                    <> multilingualText
                        (" healed " `append` pack (show (x ^. healAmount)))
                        ("は" `append` pack (show (x ^. healAmount)) `append` "ポイント回復した．")
                ]
                , True)
                , pushActor (healHp newActor (x ^. healAmount)) d
            )
        Nothing -> (([multilingualText "What do you consume?" "何を使う？"], False), pushActor e d)

    where (item, newActor) = removeNthItem n e

updatePosition :: Dungeon -> Actor -> V2 Int -> Actor
updatePosition d src offset
    = let next = nextPosition d src offset
      in if movable d next
            then src & position .~ next
            else src

movable :: Dungeon -> Coord -> Bool
movable d c = isNothing (actorAt c d) && isPositionInRange d c && (d ^. tileMap) ! c ^. walkable

nextPosition :: Dungeon -> Actor -> V2 Int -> Coord
nextPosition d src offset =
    max (V2 0 0) $ min (V2 (width - 1) $ height - 1) $ src ^. position + offset
    where V2 width height = mapWidthAndHeight d

isPositionInRange :: Dungeon -> Coord -> Bool
isPositionInRange d c = x >= 0 && x < width && y >= 0 && y < height
    where V2 width height = mapWidthAndHeight d
          V2 x y = c
