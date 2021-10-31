{-# LANGUAGE OverloadedStrings #-}

module Dungeon.Actor.Actions.Melee
    ( meleeAction
    ) where

import           Control.Lens              ((^.))
import           Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import           Control.Monad.Writer      (MonadPlus (mzero), Writer)
import           Dungeon                   (Dungeon, popActorAt, pushActor)
import           Dungeon.Actor             (Actor, position)
import qualified Dungeon.Actor             as A
import           Dungeon.Actor.Actions     (Action)
import           Linear.V2                 (V2)
import           Log                       (MessageLog)

meleeAction :: V2 Int -> Action
meleeAction offset src dungeon =
    result
    where dstPosition = (src ^. position) + offset
          (target, dungeonWithoutTarget) = popActorAt dstPosition dungeon

          result =
            case target of
                Nothing       -> mzero
                Just defender -> MaybeT $ Just <$> attackFromTo src defender dungeonWithoutTarget

attackFromTo :: Actor -> Actor -> Dungeon -> Writer MessageLog Dungeon
attackFromTo attacker defender dungeonWithoutAttackerAndDefender = do
    newDefender <- runMaybeT $ A.attackFromTo attacker defender

    return $ pushActor attacker $ maybe dungeonWithoutAttackerAndDefender (`pushActor` dungeonWithoutAttackerAndDefender) newDefender
