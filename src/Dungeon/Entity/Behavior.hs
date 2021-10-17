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

    r <- action e'
    return $ fst r

updatePath :: Entity -> Dungeon -> Entity
updatePath e d = e & pathToDestination .~ newPath
    where newPath = fromMaybe [] $ dst >>= getPathTo d src
          src = e ^. position
          dst = fmap (^. position) player
          player = getPlayerEntity d

selectAction :: Entity -> Dungeon -> Action
selectAction e d
    | isEntityNextToPlayer e d =
        case offsetToPlayer e d of
            Just offset -> meleeAction offset
            Nothing     -> moveOrWait e
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
isEntityNextToPlayer e d = case distance of
                            Just d' -> d' <= 1
                            Nothing -> False
    where distance = (\(V2 x y) -> max (abs x) (abs y)) <$> offsetToPlayer e d

offsetToPlayer :: Entity -> Dungeon -> Maybe (V2 Int)
offsetToPlayer e d = fmap (\x -> x - e ^. position) playerPos
    where p = getPlayerEntity d
          playerPos = fmap (^. position) p
