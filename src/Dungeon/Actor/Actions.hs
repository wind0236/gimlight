{-# LANGUAGE OverloadedStrings #-}

module Dungeon.Actor.Actions
    ( meleeAction
    , moveAction
    , waitAction
    , Action
    ) where

import           Control.Lens              ((&), (.~), (^.))
import           Control.Monad             (when)
import           Control.Monad.Trans.State (State, execState, state)
import           Coord                     (Coord)
import           Data.Array                ((!))
import           Data.List                 (find)
import           Data.Text                 (append, pack)
import           Dungeon                   (Dungeon, actors, mapWidthAndHeight,
                                            popActorAt, pushActor, tileMap)
import           Dungeon.Actor             (Actor, blocksMovement, defence,
                                            getHp, isPlayer, name, position,
                                            power, updateHp)
import           Dungeon.Map.Tile          (walkable)
import           Linear.V2                 (V2 (V2))
import           Log                       (MessageLog, message)

type Action = Actor -> State Dungeon (MessageLog, Bool)

meleeAction :: V2 Int -> Action
meleeAction offset src = do
        let pos = src ^. position
            dest = pos + offset

        target <- popActorAt dest

        case target of
            Nothing -> do
                pushActor src
                return ([], False)
            Just x -> let damage = src ^. power - x ^. defence
                          msg = (src ^. name) `append` " attacks " `append` (x ^. name)
                        in if damage > 0
                            then do
                                let newHp = getHp x - damage
                                    newActor = updateHp x newHp
                                    damagedMessage = msg `append` pack " for " `append` pack (show damage) `append` " hit points."
                                    deathMessage = if isPlayer x then "You died!" else (x ^. name) `append` " is dead!"
                                    messages = if newHp <= 0 then [damagedMessage, deathMessage] else [damagedMessage]

                                pushActor src

                                when (newHp > 0) $ pushActor newActor

                                return (fmap message messages, True)
                            else do
                                    pushActor src
                                    pushActor x
                                    return ([message $ msg `append` " but does not damage."], True)

moveAction :: V2 Int -> Action
moveAction offset src = state $ \d -> result d
    where result d = (\(x, y) -> (x, execState y d)) $ messageAndNewActor d
          messageAndNewActor d = if not (movable d (src ^. position + offset))
                                    then ((["That way is blocked."], False), pushActor src)
                                    else (([], True), pushActor $ updatePosition d src offset)

waitAction :: Action
waitAction e = do
        pushActor e
        return ([], True)

updatePosition :: Dungeon -> Actor -> V2 Int -> Actor
updatePosition d src offset
    = let next = nextPosition d src offset
      in if movable d next
            then src & position .~ next
            else src

movable :: Dungeon -> Coord -> Bool
movable d c = case getBlockingActorAtLocation d c of
                  Just _  -> False
                  Nothing -> isPositionInRange d c && (d ^. tileMap) ! c ^. walkable

getBlockingActorAtLocation :: Dungeon -> Coord -> Maybe Actor
getBlockingActorAtLocation d c = find (\x -> x ^. position == c && x ^. blocksMovement) (d ^. actors)

nextPosition :: Dungeon -> Actor -> V2 Int -> Coord
nextPosition d src offset =
    max (V2 0 0) $ min (V2 (width - 1) $ height - 1) $ src ^. position + offset
    where V2 width height = mapWidthAndHeight d

isPositionInRange :: Dungeon -> Coord -> Bool
isPositionInRange d c = x >= 0 && x < width && y >= 0 && y < height
    where V2 width height = mapWidthAndHeight d
          V2 x y = c