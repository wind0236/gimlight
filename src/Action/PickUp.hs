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
import           Dungeon              (popItemAt, pushActor)
import           Item                 (Item, getName)
import qualified Localization.Texts   as T

pickUpAction :: Action
pickUpAction position e _ d = maybe youGotNothing pickUpItem item
  where
    pickUpItem i =
        case addItem i (e ^. inventoryItems) of
            Just xs -> pickUpSucceed i xs
            Nothing -> bagIsFull
    pickUpSucceed :: Item -> Inventory -> ActionResultWithLog
    pickUpSucceed i newInventory = do
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
    (item, dungeonAfterPickingUp) = popItemAt position d
