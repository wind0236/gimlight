{-# LANGUAGE OverloadedStrings #-}

module Dungeon.Actor.Actions.Consume
    ( consumeAction
    ) where

import           Control.Lens          ((^.))
import           Data.Text             (append, pack)
import           Dungeon               (pushActor)
import           Dungeon.Actor         (healHp, name, removeNthItem)
import           Dungeon.Actor.Actions (Action)
import           Dungeon.Item          (healAmount)
import           Localization          (multilingualText)

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
                , pushActor (healHp (x ^. healAmount) newActor) d
            )
        Nothing -> (([multilingualText "What do you consume?" "何を使う？"], False), pushActor e d)

    where (item, newActor) = removeNthItem n e
