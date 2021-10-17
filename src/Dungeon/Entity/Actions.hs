{-# LANGUAGE OverloadedStrings #-}

module Dungeon.Entity.Actions
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
import           Dungeon                   (Dungeon, mapWidthAndHeight,
                                            popActorAt, pushEntity)
import           Dungeon.Entity            (getHp, isPlayer, updateHp)
import           Dungeon.Map.Tile          (walkable)
import           Dungeon.Types             (Entity, blocksMovement, defence,
                                            entities, name, position, power,
                                            tileMap)
import           Linear.V2                 (V2 (V2))
import           Log                       (MessageLog, message)

type Action = Entity -> State Dungeon MessageLog

meleeAction :: V2 Int -> Action
meleeAction offset src = do
        let pos = src ^. position
            dest = pos + offset

        target <- popActorAt dest

        case target of
            Nothing -> do
                pushEntity src
                return []
            Just x -> let damage = src ^. power - x ^. defence
                          msg = (src ^. name) `append` " attacks " `append` (x ^. name)
                        in if damage > 0
                            then do
                                let newHp = getHp x - damage
                                    newEntity = updateHp x newHp
                                    damagedMessage = msg `append` pack " for " `append` pack (show damage) `append` " hit points."
                                    deathMessage = if isPlayer x then "You died!" else (x ^. name) `append` " is dead!"
                                    messages = if newHp <= 0 then [damagedMessage, deathMessage] else [damagedMessage]

                                pushEntity src

                                when (newHp > 0) $ pushEntity newEntity

                                return $ fmap message messages
                            else do
                                    pushEntity src
                                    pushEntity x
                                    return [message $ msg `append` " but does not damage."]

moveAction :: V2 Int -> Action
moveAction offset src = state $ \d -> result d
    where result d = (\(x, y) -> (x, execState y d)) $ messageAndNewEntity d
          messageAndNewEntity d = if not (movable d (src ^. position + offset))
                                    then (["That way is blocked."], pushEntity src)
                                    else ([], pushEntity $ updatePosition d src offset)

waitAction :: Action
waitAction e = do
        pushEntity e
        return []

updatePosition :: Dungeon -> Entity -> V2 Int -> Entity
updatePosition d src offset
    = let next = nextPosition d src offset
      in if movable d next
            then src & position .~ next
            else src

movable :: Dungeon -> Coord -> Bool
movable d c = case getBlockingEntityAtLocation d c of
                  Just _  -> False
                  Nothing -> isPositionInRange d c && (d ^. tileMap) ! c ^. walkable

getBlockingEntityAtLocation :: Dungeon -> Coord -> Maybe Entity
getBlockingEntityAtLocation d c = find (\x -> x ^. position == c && x ^. blocksMovement) (d ^. entities)

nextPosition :: Dungeon -> Entity -> V2 Int -> Coord
nextPosition d src offset =
    max (V2 0 0) $ min (V2 (width - 1) $ height - 1) $ src ^. position + offset
    where V2 width height = mapWidthAndHeight d

isPositionInRange :: Dungeon -> Coord -> Bool
isPositionInRange d c = x >= 0 && x < width && y >= 0 && y < height
    where V2 width height = mapWidthAndHeight d
          V2 x y = c
