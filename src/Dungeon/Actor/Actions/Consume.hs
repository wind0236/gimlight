module Dungeon.Actor.Actions.Consume
    ( consumeAction
    ) where

import           Control.Lens          ((^.))
import           Control.Monad.Writer  (tell)
import           Dungeon               (pushActor)
import           Dungeon.Actor         (healHp, name, removeNthItem)
import           Dungeon.Actor.Actions (Action)
import           Dungeon.Item          (healAmount)
import qualified Localization.Texts    as T

consumeAction :: Int -> Action
consumeAction n e d =
    case item of
        Just x -> do
            tell [T.healed (e ^. name) (x ^. healAmount)]
            return (True, pushActor (healHp (x ^. healAmount) newActor) d)
        Nothing -> do
            tell [T.whatToUse]
            return (False, pushActor e d)

    where (item, newActor) = removeNthItem n e
