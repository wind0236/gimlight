module Action.Drop
    ( dropAction
    ) where

import           Action               (Action, ActionResult (ActionResult),
                                       ActionStatus (Failed, Ok))
import           Actor                (inventoryItems)
import           Control.Arrow        (Arrow (first))
import           Control.Lens         ((&), (.~), (^.))
import           Control.Monad.State  (runStateT)
import           Control.Monad.Writer (tell)
import           Data.Maybe           (fromMaybe)
import           Dungeon.Map.Cell     (Error (ItemAlreadyExists), locateActorAt,
                                       locateItemAt, removeActorAt)
import           Inventory            (removeNthItem)
import           Item                 (getName)
import qualified Localization.Texts   as T

dropAction :: Int -> Action
dropAction n position tc cm =
    case result of
        Right (dropped, newMap) -> do
            tell [T.youDropped $ getName dropped]
            return $ ActionResult Ok newMap []
        Left (ItemAlreadyExists _) -> do
            tell [T.itemExists]
            return $ ActionResult Failed cm []
        e -> error $ "Unexpected error: " <> show e
  where
    result =
        flip runStateT cm $ do
            a <- removeActorAt position
            let (item, newInventory) =
                    first (fromMaybe (error "Index out of bounds.")) $
                    removeNthItem n (a ^. inventoryItems)
            locateActorAt tc (a & inventoryItems .~ newInventory) position
            locateItemAt tc item position
            return item
