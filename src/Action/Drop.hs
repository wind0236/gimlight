module Action.Drop
    ( dropAction
    ) where

import           Action               (Action, ActionResult (ActionResult),
                                       ActionResultWithLog,
                                       ActionStatus (Failed, Ok))
import           Actor                (removeNthItem)
import           Control.Lens         ((^.))
import           Control.Monad.Writer (tell)
import           Data.Maybe           (isJust)
import           Dungeon              (cellMap, pushActor, pushItem)
import           Dungeon.Map.Cell     (removeItemAt)
import           Item                 (Item, getName)
import           Localization         (MultilingualText)
import qualified Localization.Texts   as T

dropAction :: Int -> Action
dropAction n position e tiles d
    | itemExists = failWithReason T.itemExists
    | otherwise =
        case item of
            Just x  -> dropItem x position newActor tiles d
            Nothing -> failWithReason T.whatToDrop
  where
    (item, newActor) = removeNthItem n e
    itemExists = isJust $ removeItemAt position $ d ^. cellMap
    failWithReason :: MultilingualText -> ActionResultWithLog
    failWithReason reason = do
        tell [reason]
        return failedResult
    failedResult = ActionResult Failed (pushActor position e d) []

dropItem :: Item -> Action
dropItem item position actor _ dungeon = do
    tell [T.youDropped $ getName item]
    return $
        ActionResult
            Ok
            (pushActor position actor $ pushItem position item dungeon)
            []
