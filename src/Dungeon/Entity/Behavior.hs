{-# LANGUAGE OverloadedStrings #-}

module Dungeon.Entity.Behavior
    ( bumpAction
    , meleeAction
    , waitAction
    , enemyAction
    , BumpResult(..)
    ) where

import           Control.Lens              (use, (&), (.~), (^.))
import           Control.Monad.Trans.State (State, execState, get, state)
import           Coord                     (Coord)
import           Data.Array                ((!))
import           Data.List                 (find)
import           Data.Maybe                (fromMaybe)
import           Data.Text                 (append, pack)
import           Dungeon                   (Dungeon, getPlayerEntity, isTown,
                                            mapWidthAndHeight, popActorAt,
                                            pushEntity)
import           Dungeon.Entity            (getHp, updateHp)
import           Dungeon.Map.Tile          (walkable)
import           Dungeon.PathFinder        (getPathTo)
import           Dungeon.Types             (Ai (HostileEnemy, _path), Entity,
                                            ai, blocksMovement, defence,
                                            entities, isAlive, isEnemy,
                                            isPlayer, name, path, position,
                                            power, talkMessage, tileMap,
                                            visible)
import           Linear.V2                 (V2 (..), _x, _y)
import           Log                       (Message, message)
import           Talking                   (TalkWith, talkWith)

data BumpResult = LogReturned [Message]
                | TalkStarted TalkWith
                | ExitToGlobalMap Entity
                | Ok

enemyAction :: Entity -> State Dungeon BumpResult
enemyAction e = do
        u <- updatePathOrMelee e

        case u of
            Right e' -> moveOrWait e'
            Left m   -> return $ LogReturned m

updatePathOrMelee :: Entity -> State Dungeon (Either [Message] Entity)
updatePathOrMelee e = do
        d <- get

        let p = getPlayerEntity d
            pos = e ^. position
            posDiff = p ^. position - pos
            distance = max (abs posDiff ^. _x) (abs posDiff ^. _y)

        v <- use visible

        if v ! pos
            then if distance <= 1
                     then do
                         msg <- meleeAction e posDiff
                         return $ Left msg

                     else do
                         newPath <- getPathTo pos (p ^. position)

                         let newAi = HostileEnemy { _path = fromMaybe [] newPath}
                             newEntity = e & ai .~ newAi

                         return $ Right newEntity
            else return $ Right e

moveOrWait :: Entity -> State Dungeon BumpResult
moveOrWait e =
        let p = e ^. ai . path
        in if null p
            then do
                    waitAction e
                    return Ok
            else let (nextCoord, remaining) = (head p, tail p)
                     offset = nextCoord - e ^. position
                     newAi = HostileEnemy { _path = remaining }
                     newEntity = e & ai .~ newAi
                 in
                     moveAction newEntity offset

bumpAction :: Entity -> V2 Int -> State Dungeon BumpResult
bumpAction src offset = do
        d <- get

        let x = getAliveActorAtLocation d (src ^. position + offset)

        case x of
            Just e
                | e ^. isEnemy -> do
                    logs <- meleeAction src offset
                    return $ LogReturned logs
                | otherwise -> do
                    pushEntity src
                    return $ TalkStarted $ talkWith e (e ^. talkMessage)
            Nothing ->
                moveAction src offset

getBlockingEntityAtLocation :: Dungeon -> Coord -> Maybe Entity
getBlockingEntityAtLocation d c = find (\x -> x ^. position == c && x ^. blocksMovement) (d ^. entities)

getAliveActorAtLocation :: Dungeon -> Coord -> Maybe Entity
getAliveActorAtLocation d c = find (\x -> x ^. position == c && x ^. isAlive) $ d ^. entities

meleeAction :: Entity -> V2 Int -> State Dungeon [Message]
meleeAction src offset = do
        let pos = src ^. position
            dest = pos + offset

        target <- popActorAt dest

        case target of
            Nothing -> do
                pushEntity src
                return []
            Just x | getHp x == 0 -> do
                pushEntity src
                pushEntity x
                return []
            Just x -> let damage = src ^. power - x ^. defence
                          msg = (src ^. name) `append` " attacks " `append` (x ^. name)
                        in if damage > 0
                            then do
                                let newHp = getHp x - damage
                                    newEntity = updateHp x newHp
                                    damagedMessage = msg `append` pack " for " `append` pack (show damage) `append` " hit points."
                                    deathMessage = if x ^. isPlayer then "You died!" else (x ^. name) `append` " is dead!"
                                    messages = if newHp <= 0 then [damagedMessage, deathMessage] else [damagedMessage]
                                pushEntity src
                                pushEntity newEntity
                                return $ fmap message messages
                            else do
                                    pushEntity src
                                    pushEntity x
                                    return [message $ msg `append` " but does not damage."]

moveAction :: Entity -> V2 Int -> State Dungeon BumpResult
moveAction src offset = state $ \d -> if not (isPositionInRange d (src ^. position + offset)) && isTown d
                                        then (ExitToGlobalMap src, d)
                                        else (Ok, execState (pushEntity $ updatePosition d src offset) d)

waitAction :: Entity -> State Dungeon ()
waitAction = pushEntity

updatePosition :: Dungeon -> Entity -> V2 Int -> Entity
updatePosition d src offset
    = let next = nextPosition d src offset
      in if movable d next
            then src & position .~ next
            else src

nextPosition :: Dungeon -> Entity -> V2 Int -> Coord
nextPosition d src offset =
    max (V2 0 0) $ min (V2 (width - 1) $ height - 1) $ src ^. position + offset
    where V2 width height = mapWidthAndHeight d

movable :: Dungeon -> Coord -> Bool
movable d c = case getBlockingEntityAtLocation d c of
                  Just _  -> False
                  Nothing -> isPositionInRange d c && (d ^. tileMap) ! c ^. walkable

isPositionInRange :: Dungeon -> Coord -> Bool
isPositionInRange d c = x >= 0 && x < width && y >= 0 && y < height
    where V2 width height = mapWidthAndHeight d
          V2 x y = c
