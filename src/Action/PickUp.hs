module Action.PickUp
    ( pickUpAction
    ) where

import           Action               (Action, ActionResult (ActionResult),
                                       ActionResultWithLog,
                                       ActionStatus (Failed, Ok))
import           Actor                (Actor, inventoryItems)
import           Actor.Inventory      (Inventory, addItem)
import           Control.Lens         ((&), (.~), (^.))
import           Control.Monad.Writer (tell)
import           Data.Maybe           (fromMaybe)
import           Dungeon.Map.Cell     (CellMap, locateActorAt, removeActorAt,
                                       removeItemAt)
import           Item                 (Item, getName)
import qualified Localization.Texts   as T

pickUpAction :: Action
pickUpAction position _ cm =
    case removeActorAt position cm of
        Just (a, ncm) -> pickUpForActor a ncm
        Nothing       -> return failedResult
  where
    pickUpForActor a ncm =
        case removeItemAt position ncm of
            Just (item, cellMapWithoutItem) ->
                pickUpItem item a cellMapWithoutItem
            Nothing -> youGotNothing
    pickUpItem i a dungeonWithoutItem =
        case addItem i (a ^. inventoryItems) of
            Just xs -> pickUpSucceed a i xs dungeonWithoutItem
            Nothing -> bagIsFull
    pickUpSucceed ::
           Actor -> Item -> Inventory -> CellMap -> ActionResultWithLog
    pickUpSucceed a i newInventory cellMapAfterPickingUp = do
        tell [T.youGotItem $ getName i]
        return $
            ActionResult
                Ok
                (fromMaybe
                     (error "Failed to locate an actor.")
                     (locateActorAt
                          (a & inventoryItems .~ newInventory)
                          position
                          cellMapAfterPickingUp))
                []
    bagIsFull = do
        tell [T.bagIsFull]
        return failedResult
    youGotNothing = do
        tell [T.youGotNothing]
        return failedResult
    failedResult = ActionResult Failed cm []
