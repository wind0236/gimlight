module Action.Drop
    ( dropAction
    ) where

import           Action               (Action, ActionResult (ActionResult),
                                       ActionResultWithLog,
                                       ActionStatus (Failed, Ok))
import           Actor                (Actor, inventoryItems)
import           Control.Lens         ((&), (.~), (^.))
import           Control.Monad.Writer (tell)
import           Data.Maybe           (fromMaybe)
import           Dungeon.Map.Cell     (CellMap, locateActorAt, locateItemAt,
                                       removeActorAt)
import           Inventory            (removeNthItem)
import           Item                 (Item, getName)
import           Localization         (MultilingualText)
import qualified Localization.Texts   as T

dropAction :: Int -> Action
dropAction n position tc cm =
    case removeActorAt position cm of
        Just (a, ncm) -> dropActionForActor a ncm
        Nothing       -> return $ ActionResult Failed cm []
  where
    dropActionForActor a ncm =
        case removeNthItem n (a ^. inventoryItems) of
            (Just item, newInventory) ->
                dropItem ncm item (a & inventoryItems .~ newInventory)
            (Nothing, _) -> failWithReason T.whatToDrop
    dropItem :: CellMap -> Item -> Actor -> ActionResultWithLog
    dropItem ncm item' a =
        case locateItemAt tc item' position ncm of
            Just newCellMap -> successResult newCellMap item' a
            Nothing         -> failWithReason T.itemExists
    successResult :: CellMap -> Item -> Actor -> ActionResultWithLog
    successResult newCellMap item' a = do
        tell [T.youDropped $ getName item']
        return $
            ActionResult
                Ok
                (fromMaybe
                     (error "Failed to locate an actor.")
                     (locateActorAt tc a position newCellMap))
                []
    failWithReason :: MultilingualText -> ActionResultWithLog
    failWithReason reason = do
        tell [reason]
        return failedResult
    failedResult = ActionResult Failed cm []
