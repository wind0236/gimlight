module Action.PickUp
    ( pickUpAction
    ) where

import           Action               (Action, ActionResult (ActionResult),
                                       ActionResultWithLog,
                                       ActionStatus (Failed, Ok))
import           Actor                (inventoryItems)
import           Actor.Inventory      (Inventory, addItem)
import           Control.Lens         ((&), (.~), (^.))
import           Control.Monad.Writer (tell)
import           Data.Maybe           (fromMaybe)
import           Dungeon.Map.Cell     (CellMap, locateActorAt, removeItemAt)
import           Item                 (Item, getName)
import qualified Localization.Texts   as T

pickUpAction :: Action
pickUpAction position e _ cm =
    case removeResult of
        Just (item, cellMapWithoutItem) -> pickUpItem item cellMapWithoutItem
        Nothing                         -> youGotNothing
  where
    pickUpItem i dungeonWithoutItem =
        case addItem i (e ^. inventoryItems) of
            Just xs -> pickUpSucceed i xs dungeonWithoutItem
            Nothing -> bagIsFull
    pickUpSucceed :: Item -> Inventory -> CellMap -> ActionResultWithLog
    pickUpSucceed i newInventory cellMapAfterPickingUp = do
        tell [T.youGotItem $ getName i]
        return $
            ActionResult
                Ok
                (fromMaybe
                     (error "Failed to locate an actor.")
                     (locateActorAt
                          (e & inventoryItems .~ newInventory)
                          position
                          cellMapAfterPickingUp))
                []
    bagIsFull = do
        tell [T.bagIsFull]
        return $ ActionResult Failed failedCellMap []
    youGotNothing = do
        tell [T.youGotNothing]
        return $ ActionResult Failed failedCellMap []
    failedCellMap =
        fromMaybe
            (error "Failed to locate an actor.")
            (locateActorAt e position cm)
    removeResult = removeItemAt position cm
