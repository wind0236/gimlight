module Dungeon.Actor.Actions.PickUp
    ( pickUpAction
    ) where

import           Control.Lens            ((&), (.~), (^.))
import           Control.Monad.Writer    (MonadPlus (mzero), tell)
import           Dungeon                 (popItemAt, pushActor)
import           Dungeon.Actor           (inventoryItems, position)
import           Dungeon.Actor.Actions   (Action)
import           Dungeon.Actor.Inventory (addItem)
import           Dungeon.Item            (getName)
import qualified Localization.Texts      as T

pickUpAction :: Action
pickUpAction e d =
    case item of
        Just x ->
            case addItem x (e ^. inventoryItems) of
                Just xs -> do
                    tell [T.youGotItem $ getName x]
                    return $ pushActor (e & inventoryItems .~ xs) dungeonAfterPickingUp
                Nothing -> do
                    tell [T.bagIsFull]
                    mzero
        Nothing -> do
            tell [T.youGotNohing]
            mzero
    where (item, dungeonAfterPickingUp) = popItemAt (e ^. position) d
