module Actor.Actions.Drop
    ( dropAction
    ) where

import           Actor                (position, removeNthItem)
import           Actor.Actions        (Action, ActionStatus (Failed, Ok))
import           Control.Lens         ((^.))
import           Control.Monad.Writer (tell)
import           Dungeon              (pushActor, pushItem)
import           Dungeon.Item         (Item, getName, setPosition)
import qualified Localization.Texts   as T

dropAction :: Int -> Action
dropAction n e d =
    case item of
        Just x -> dropItem x newActor d
        Nothing -> do
            tell [T.whatToDrop]
            return (Failed, pushActor e d)
  where
    (item, newActor) = removeNthItem n e

dropItem :: Item -> Action
dropItem item actor dungeon = do
    tell [T.youDropped $ getName item]
    return (Ok, pushActor actor $ pushItem itemWithNewPosition dungeon)
  where
    itemWithNewPosition = setPosition (actor ^. position) item
