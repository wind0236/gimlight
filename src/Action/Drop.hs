module Action.Drop
    ( dropAction
    ) where

import           Action               (Action, ActionResult (ActionResult),
                                       ActionStatus (Failed, Ok))
import           Actor                (removeNthItem)
import           Control.Monad.Writer (tell)
import           Dungeon              (pushActor, pushItem)
import           Item                 (Item, getName)
import qualified Localization.Texts   as T

dropAction :: Int -> Action
dropAction n position e tiles d =
    case item of
        Just x -> dropItem x position newActor tiles d
        Nothing -> do
            tell [T.whatToDrop]
            return $ ActionResult Failed (pushActor position e d) []
  where
    (item, newActor) = removeNthItem n e

dropItem :: Item -> Action
dropItem item position actor _ dungeon = do
    tell [T.youDropped $ getName item]
    return $
        ActionResult
            Ok
            (pushActor position actor $ pushItem position item dungeon)
            []
