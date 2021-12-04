module Action.Drop
    ( dropAction
    ) where

import           Action               (Action, ActionResult (ActionResult),
                                       ActionResultWithLog,
                                       ActionStatus (Failed, Ok))
import           Actor                (removeNthItem)
import           Control.Lens         ((&), (.~), (^.))
import           Control.Monad.Writer (tell)
import           Dungeon              (cellMap, pushActor)
import           Dungeon.Map.Cell     (CellMap, locateItemAt)
import           Item                 (Item, getName)
import           Localization         (MultilingualText)
import qualified Localization.Texts   as T

dropAction :: Int -> Action
dropAction n position e _ d =
    case item of
        Just x  -> dropItem x
        Nothing -> failWithReason T.whatToDrop
  where
    (item, newActor) = removeNthItem n e
    dropItem :: Item -> ActionResultWithLog
    dropItem item' =
        case locateItemAt item' position (d ^. cellMap) of
            Just newCellMap -> successResult newCellMap item'
            Nothing         -> failWithReason T.itemExists
    successResult :: CellMap -> Item -> ActionResultWithLog
    successResult newCellMap item' = do
        tell [T.youDropped $ getName item']
        return $
            ActionResult
                Ok
                (pushActor position newActor (d & cellMap .~ newCellMap))
                []
    failWithReason :: MultilingualText -> ActionResultWithLog
    failWithReason reason = do
        tell [reason]
        return failedResult
    failedResult = ActionResult Failed (pushActor position e d) []
