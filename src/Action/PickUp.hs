module Action.PickUp
    ( pickUpAction
    ) where

import           Action               (Action, ActionStatus (Failed, Ok))
import           Actor                (inventoryItems, position)
import           Actor.Inventory      (addItem)
import           Control.Lens         ((&), (.~), (^.))
import           Control.Monad.Writer (tell)
import           Dungeon              (popItemAt, pushActor)
import           Item                 (getName)
import qualified Localization.Texts   as T

pickUpAction :: Action
pickUpAction e d =
    case item of
        Just x ->
            case addItem x (e ^. inventoryItems) of
                Just xs -> do
                    tell [T.youGotItem $ getName x]
                    return
                        ( Ok
                        , pushActor
                              (e & inventoryItems .~ xs)
                              dungeonAfterPickingUp)
                Nothing -> do
                    tell [T.bagIsFull]
                    return (Failed, pushActor e d)
        Nothing -> do
            tell [T.youGotNohing]
            return (Failed, pushActor e d)
  where
    (item, dungeonAfterPickingUp) = popItemAt (e ^. position) d
