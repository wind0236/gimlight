module Dungeon.Actor.Actions.Consume
    ( consumeAction
    ) where

import           Control.Lens          ((^.))
import           Control.Monad.Writer  (MonadPlus (mzero), tell)
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
            return $ pushActor (healHp (x ^. healAmount) newActor) d
        Nothing -> do
            tell [T.whatToUse]
            mzero

    where (item, newActor) = removeNthItem n e
