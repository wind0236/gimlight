module Action.Drop
    ( dropAction
    ) where

import           Action               (Action, ActionResult (ActionResult),
                                       ActionResultWithLog,
                                       ActionStatus (Failed, Ok))
import           Actor                (removeNthItem)
import           Control.Monad.Writer (tell)
import           Data.Maybe           (fromMaybe)
import           Dungeon.Map.Cell     (CellMap, locateActorAt, locateItemAt)
import           Item                 (Item, getName)
import           Localization         (MultilingualText)
import qualified Localization.Texts   as T

dropAction :: Int -> Action
dropAction n position e _ cm =
    case item of
        Just x  -> dropItem x
        Nothing -> failWithReason T.whatToDrop
  where
    (item, newActor) = removeNthItem n e
    dropItem :: Item -> ActionResultWithLog
    dropItem item' =
        case locateItemAt item' position cm of
            Just newCellMap -> successResult newCellMap item'
            Nothing         -> failWithReason T.itemExists
    successResult :: CellMap -> Item -> ActionResultWithLog
    successResult newCellMap item' = do
        tell [T.youDropped $ getName item']
        return $
            ActionResult
                Ok
                (fromMaybe
                     (error "Failed to locate an actor.")
                     (locateActorAt newActor position newCellMap))
                []
    failWithReason :: MultilingualText -> ActionResultWithLog
    failWithReason reason = do
        tell [reason]
        return failedResult
    failedResult =
        ActionResult
            Failed
            (fromMaybe
                 (error "Failed to locate an actor.")
                 (locateActorAt e position cm))
            []
