{-# LANGUAGE OverloadedStrings #-}

module Dungeon.Entity.Behavior
    ( npcAction
    ) where

import           Control.Lens              ((&), (.~), (^.))
import           Control.Monad.Trans.State (State, get)
import           Data.Maybe                (fromMaybe)
import           Dungeon                   (Dungeon, getPlayerEntity)
import           Dungeon.Entity.Actions    (Action, meleeAction, moveAction,
                                            waitAction)
import           Dungeon.PathFinder        (getPathTo)
import           Dungeon.Types             (Entity, pathToDestination, position)
import           Linear.V2                 (V2 (V2))
import           Log                       (MessageLog)

npcAction :: Entity -> State Dungeon MessageLog
npcAction e = do
    dungeon <- get

    let e' = updatePath e dungeon
        action = selectAction e' dungeon

    action e'

updatePath :: Entity -> Dungeon -> Entity
updatePath e d = e & pathToDestination .~ newPath
    where newPath = fromMaybe [] $ getPathTo d src dst
          src = e ^. position
          dst = player ^. position
          player = getPlayerEntity d

selectAction :: Entity -> Dungeon -> Action
selectAction e d
    | isEntityNextToPlayer e d = meleeAction (offsetToPlayer e d)
    | otherwise = moveOrWait e

moveOrWait :: Entity -> Action
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

isEntityNextToPlayer :: Entity -> Dungeon -> Bool
isEntityNextToPlayer e d = distance <= 1
    where distance = max (abs x) (abs y)
          V2 x y = p ^. position - e ^. position
          p = getPlayerEntity d

offsetToPlayer :: Entity -> Dungeon -> V2 Int
offsetToPlayer e d = p ^. position - e ^. position
    where p = getPlayerEntity d
