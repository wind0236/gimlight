module Dungeon.Actor.Actions.PickUp
    ( pickUpAction
    ) where

import           Control.Lens            ((&), (.~), (^.))
import           Control.Monad.Writer    (tell)
import           Dungeon                 (popItemAt, pushActor, pushItem)
import           Dungeon.Actor           (inventoryItems, position)
import           Dungeon.Actor.Actions   (Action)
import           Dungeon.Actor.Inventory (addItem)
import           Dungeon.Item            (name)
import qualified Localization.Texts      as T

pickUpAction :: Action
pickUpAction e d =
    case item of
        Just x ->
            case addItem x (e ^. inventoryItems) of
                Just xs -> do
                    tell [T.youGotItem $ x ^. name]
                    return (True, pushActor (e & inventoryItems .~ xs) dungeonAfterPickingUp)
                Nothing -> do
                    tell [T.bagIsFull]
                    return (False, pushItem x $ pushActor e dungeonAfterPickingUp)
        Nothing -> do
            tell [T.youGotNohing]
            return (False, pushActor e dungeonAfterPickingUp)
    where (item, dungeonAfterPickingUp) = popItemAt (e ^. position) d
