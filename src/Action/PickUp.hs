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
import           Dungeon              (Dungeon, cellMap, pushActor)
import           Dungeon.Map.Cell     (removeItemAt)
import           Item                 (Item, getName)
import qualified Localization.Texts   as T

pickUpAction :: Action
pickUpAction position e _ d =
    case removeResult of
        Just (item, cellMapWithoutItem) ->
            pickUpItem item (d & cellMap .~ cellMapWithoutItem)
        Nothing -> youGotNothing
  where
    pickUpItem i dungeonWithoutItem =
        case addItem i (e ^. inventoryItems) of
            Just xs -> pickUpSucceed i xs dungeonWithoutItem
            Nothing -> bagIsFull
    pickUpSucceed :: Item -> Inventory -> Dungeon -> ActionResultWithLog
    pickUpSucceed i newInventory dungeonAfterPickingUp = do
        tell [T.youGotItem $ getName i]
        return $
            ActionResult
                Ok
                (pushActor
                     position
                     (e & inventoryItems .~ newInventory)
                     dungeonAfterPickingUp)
                []
    bagIsFull = do
        tell [T.bagIsFull]
        return $ ActionResult Failed (pushActor position e d) []
    youGotNothing = do
        tell [T.youGotNothing]
        return $ ActionResult Failed (pushActor position e d) []
    removeResult = removeItemAt position (d ^. cellMap)
