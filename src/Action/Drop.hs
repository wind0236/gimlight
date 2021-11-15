module Action.Drop
    ( dropAction
    ) where

import           Action               (Action, ActionResult (ActionResult),
                                       ActionStatus (Failed, Ok))
import           Actor                (position, removeNthItem)
import           Control.Lens         ((^.))
import           Control.Monad.Writer (tell)
import           Dungeon              (pushActor, pushItem)
import           Item                 (Item, getName, setPosition)
import qualified Localization.Texts   as T

dropAction :: Int -> Action
dropAction n e d =
    case item of
        Just x -> dropItem x newActor d
        Nothing -> do
            tell [T.whatToDrop]
            return $ ActionResult Failed (pushActor e d) []
  where
    (item, newActor) = removeNthItem n e

dropItem :: Item -> Action
dropItem item actor dungeon = do
    tell [T.youDropped $ getName item]
    return $
        ActionResult
            Ok
            (pushActor actor $ pushItem itemWithNewPosition dungeon)
            []
  where
    itemWithNewPosition = setPosition (actor ^. position) item
