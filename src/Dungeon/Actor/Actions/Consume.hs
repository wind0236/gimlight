module Dungeon.Actor.Actions.Consume
    ( consumeAction
    ) where

import           Control.Lens          ((^.))
import           Control.Monad.Writer  (tell)
import           Dungeon               (pushActor)
import           Dungeon.Actor         (healHp, name, removeNthItem)
import           Dungeon.Actor.Actions (Action,
                                        ActionStatus (Failed, Ok, ReadingStarted))
import           Dungeon.Item          (Effect (Book, Heal), getEffect,
                                        isUsableManyTimes)
import           Dungeon.Item.Heal     (getHealAmount)
import qualified Localization.Texts    as T

consumeAction :: Int -> Action
consumeAction n e d =
    case item of
        Just x -> useItem x
        Nothing -> do
            tell [T.whatToUse]
            return (Failed, pushActor e d)
  where
    useItem x =
        let actor =
                if isUsableManyTimes x
                    then e
                    else newActor
         in doItemEffect (getEffect x) actor d
    (item, newActor) = removeNthItem n e

doItemEffect :: Effect -> Action
doItemEffect (Heal handler) actor dungeon = do
    tell [T.healed (actor ^. name) amount]
    return (Ok, pushActor (healHp amount actor) dungeon)
  where
    amount = getHealAmount handler
doItemEffect (Book handler) actor dungeon =
    return (ReadingStarted handler, pushActor actor dungeon)
