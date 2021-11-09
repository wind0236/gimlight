module Action.Consume
    ( consumeAction
    ) where

import           Action               (Action, ActionResult (ActionResult),
                                       ActionStatus (Failed, Ok, ReadingStarted))
import           Actor                (healHp, name, removeNthItem)
import           Control.Lens         ((^.))
import           Control.Monad.Writer (tell)
import           Dungeon              (pushActor)
import           Item                 (Effect (Book, Heal), getEffect,
                                       isUsableManyTimes)
import           Item.Heal            (getHealAmount)
import qualified Localization.Texts   as T

consumeAction :: Int -> Action
consumeAction n e d =
    case item of
        Just x -> useItem x
        Nothing -> do
            tell [T.whatToUse]
            return $ ActionResult Failed $ pushActor e d
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
    return $ ActionResult Ok $ pushActor (healHp amount actor) dungeon
  where
    amount = getHealAmount handler
doItemEffect (Book handler) actor dungeon =
    return $ ActionResult (ReadingStarted handler) $ pushActor actor dungeon
