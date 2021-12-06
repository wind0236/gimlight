module Action.Consume
    ( consumeAction
    ) where

import           Action               (Action, ActionResult (ActionResult),
                                       ActionStatus (Failed, Ok, ReadingStarted))
import           Actor                (getIdentifier, healHp, removeNthItem)
import           Actor.Identifier     (toName)
import           Control.Monad.Writer (tell)
import           Data.Maybe           (fromMaybe)
import           Dungeon.Map.Cell     (locateActorAt)
import           Item                 (Effect (Book, Heal), getEffect,
                                       isUsableManyTimes)
import           Item.Heal            (getHealAmount)
import qualified Localization.Texts   as T

consumeAction :: Int -> Action
consumeAction n position e tiles cm =
    case item of
        Just x -> useItem x
        Nothing -> do
            tell [T.whatToUse]
            return $
                ActionResult
                    Failed
                    (fromMaybe
                         (error "Failed to locate an actor.")
                         (locateActorAt e position cm))
                    []
  where
    useItem x =
        let actor =
                if isUsableManyTimes x
                    then e
                    else newActor
         in doItemEffect (getEffect x) position actor tiles cm
    (item, newActor) = removeNthItem n e

doItemEffect :: Effect -> Action
doItemEffect (Heal handler) position actor _ cm = do
    tell [T.healed (toName $ getIdentifier actor) amount]
    return $
        ActionResult
            Ok
            (fromMaybe
                 (error "Failed to locate an actor.")
                 (locateActorAt (healHp amount actor) position cm))
            []
  where
    amount = getHealAmount handler
doItemEffect (Book handler) position actor _ cm =
    return $
    ActionResult
        (ReadingStarted handler)
        (fromMaybe
             (error "Failed to locate an actor.")
             (locateActorAt actor position cm))
        []
