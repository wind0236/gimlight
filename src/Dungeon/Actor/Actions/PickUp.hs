{-# LANGUAGE OverloadedStrings #-}

module Dungeon.Actor.Actions.PickUp
    ( pickUpAction
    ) where

import           Control.Lens            ((&), (.~), (^.))
import           Dungeon                 (popItemAt, pushActor, pushItem)
import           Dungeon.Actor           (inventoryItems, position)
import           Dungeon.Actor.Actions   (Action)
import           Dungeon.Actor.Inventory (addItem)
import           Dungeon.Item            (name)
import           Localization            (multilingualText)

pickUpAction :: Action
pickUpAction e d =
    case item of
        Just x ->
            case addItem x (e ^. inventoryItems) of
                Just xs ->
                    (
                        ([multilingualText "You got " "アイテムを入手しました：" <> (x ^. name)], True),
                        pushActor (e & inventoryItems .~ xs) dungeonAfterPickingUp
                    )
                Nothing ->
                    (([multilingualText "Your bag is full." "バッグは一杯だ．"], False), pushItem x $ pushActor e dungeonAfterPickingUp)
        Nothing -> (([multilingualText "You got nothing." "あなたは無を手に入れた．"], False), pushActor e dungeonAfterPickingUp)
    where (item, dungeonAfterPickingUp) = popItemAt (e ^. position) d
