module Action.Consume
    ( consumeAction
    ) where

import           Action               (Action, ActionResult (ActionResult),
                                       ActionResultWithLog,
                                       ActionStatus (Failed, Ok, ReadingStarted))
import           Actor                (Actor, getIdentifier, healHp,
                                       inventoryItems)
import           Actor.Identifier     (toName)
import           Control.Lens         ((&), (.~), (^.))
import           Control.Monad.Writer (tell)
import           Data.Maybe           (fromMaybe)
import           Dungeon.Map.Cell     (CellMap, locateActorAt, removeActorAt)
import           Inventory            (removeNthItem)
import           Item                 (Effect (Book, Heal), getEffect,
                                       isUsableManyTimes)
import           Item.Heal            (getHealAmount)
import qualified Localization.Texts   as T

consumeAction :: Int -> Action
consumeAction n position tc cm =
    case removeActorAt position cm of
        Just (a, ncm) -> consumeActionForActor a ncm
        Nothing       -> return $ ActionResult Failed cm []
  where
    consumeActionForActor actorWithItem ncm =
        case removeNthItem n (actorWithItem ^. inventoryItems) of
            (Just x, i) ->
                useItem
                    x
                    (actorWithItem & inventoryItems .~ i)
                    actorWithItem
                    ncm
            (Nothing, _) -> do
                tell [T.whatToUse]
                return $ ActionResult Failed cm []
    useItem x actorWithoutItem actorWithItem ncm =
        let actor =
                if isUsableManyTimes x
                    then actorWithItem
                    else actorWithoutItem
         in doItemEffect (getEffect x) actor ncm
    doItemEffect :: Effect -> Actor -> CellMap -> ActionResultWithLog
    doItemEffect (Heal handler) a ncm = do
        tell [T.healed (toName $ getIdentifier a) amount]
        return $
            ActionResult
                Ok
                (fromMaybe
                     (error "Failed to locate an actor.")
                     (locateActorAt tc (healHp amount a) position ncm))
                []
      where
        amount = getHealAmount handler
    doItemEffect (Book handler) a ncm =
        return $
        ActionResult
            (ReadingStarted handler)
            (fromMaybe
                 (error "Failed to locate an actor.")
                 (locateActorAt tc a position ncm))
            []
