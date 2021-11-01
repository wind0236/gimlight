{-# LANGUAGE OverloadedStrings #-}

module Dungeon.Actor.NpcBehavior
    ( npcAction
    ) where

import           Control.Lens                ((&), (.~), (^.))
import           Control.Monad.Trans.Maybe   (MaybeT)
import           Control.Monad.Writer        (Writer)
import           Data.Maybe                  (fromMaybe)
import           Dungeon                     (Dungeon, getPlayerActor)
import           Dungeon.Actor               (Actor, pathToDestination,
                                              position)
import           Dungeon.Actor.Actions       (Action)
import           Dungeon.Actor.Actions.Melee (meleeAction)
import           Dungeon.Actor.Actions.Move  (moveAction)
import           Dungeon.Actor.Actions.Wait  (waitAction)
import           Dungeon.PathFinder          (getPathTo)
import           Linear.V2                   (V2 (V2))
import           Log                         (MessageLog)

npcAction :: Actor -> Dungeon -> MaybeT (Writer MessageLog) Dungeon
npcAction e d = action entityAfterUpdatingPath d
    where entityAfterUpdatingPath = updatePath e d
          action = selectAction entityAfterUpdatingPath d

updatePath :: Actor -> Dungeon -> Actor
updatePath e d = e & pathToDestination .~ newPath
    where newPath = fromMaybe [] $ dst >>= getPathTo d src
          src = e ^. position
          dst = fmap (^. position) player
          player = getPlayerActor d

selectAction :: Actor -> Dungeon -> Action
selectAction e d
    | isActorNextToPlayer e d =
        case offsetToPlayer e d of
            Just offset -> meleeAction offset
            Nothing     -> moveOrWait e
    | otherwise = moveOrWait e

moveOrWait :: Actor -> Action
moveOrWait e =
    if null $ e ^. pathToDestination
        then waitAction
        else popPathToDestinationAndMove

popPathToDestinationAndMove :: Action
popPathToDestinationAndMove e =
    moveAction offset e'
    where offset = next - e ^. position
          (next, remaining) = (head path, tail path)
          path = e ^. pathToDestination
          e' = e & pathToDestination .~ remaining

isActorNextToPlayer :: Actor -> Dungeon -> Bool
isActorNextToPlayer e d = case distance of
                            Just d' -> d' <= 1
                            Nothing -> False
    where distance = (\(V2 x y) -> max (abs x) (abs y)) <$> offsetToPlayer e d

offsetToPlayer :: Actor -> Dungeon -> Maybe (V2 Int)
offsetToPlayer e d = fmap (\x -> x - e ^. position) playerPos
    where p = getPlayerActor d
          playerPos = fmap (^. position) p
