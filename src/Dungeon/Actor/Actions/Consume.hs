module Dungeon.Actor.Actions.Consume
    ( consumeAction
    ) where

import           Control.Lens          ((^.))
import           Dungeon               (pushActor)
import           Dungeon.Actor         (healHp, name, removeNthItem)
import           Dungeon.Actor.Actions (Action)
import           Dungeon.Item          (healAmount)
import qualified Localization.Texts    as T

consumeAction :: Int -> Action
consumeAction n e d =
    case item of
        Just x ->
            (
                ([T.healed (e ^. name) (x ^. healAmount)]
                , True)
                , pushActor (healHp (x ^. healAmount) newActor) d
            )
        Nothing -> (([T.whatToUse], False), pushActor e d)

    where (item, newActor) = removeNthItem n e
