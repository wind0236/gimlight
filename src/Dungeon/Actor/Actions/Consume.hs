module Dungeon.Actor.Actions.Consume
    ( consumeAction
    ) where

import           Control.Lens          ((^.))
import           Control.Monad.Writer  (MonadPlus (mzero), tell)
import           Dungeon               (pushActor)
import           Dungeon.Actor         (healHp, name, removeNthItem)
import           Dungeon.Actor.Actions (Action)
import           Dungeon.Item          (Effect (Heal), getEffect)
import           Dungeon.Item.Heal     (getHealAmount)
import qualified Localization.Texts    as T

consumeAction :: Int -> Action
consumeAction n e d =
    case item of
        Just x -> doItemEffect (getEffect x) newActor d
        Nothing -> do
            tell [T.whatToUse]
            mzero
    where (item, newActor) = removeNthItem n e

doItemEffect :: Effect -> Action
doItemEffect (Heal handler) actor dungeon = do
    tell [T.healed (actor ^. name) amount]

    return $ pushActor (healHp amount actor) dungeon

    where amount = getHealAmount handler
