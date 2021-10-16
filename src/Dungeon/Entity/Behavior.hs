{-# LANGUAGE OverloadedStrings #-}

module Dungeon.Entity.Behavior
    ( npcAction
    ) where

import           Control.Lens              (use, (&), (.~), (^.))
import           Control.Monad.Trans.State (State, get)
import           Data.Array                ((!))
import           Data.Maybe                (fromMaybe)
import           Dungeon                   (Dungeon, getPlayerEntity)
import           Dungeon.Entity.Actions    (meleeAction, moveAction, waitAction)
import           Dungeon.PathFinder        (getPathTo)
import           Dungeon.Types             (Entity, pathToDestination, position,
                                            visible)
import           Linear.V2                 (_x, _y)
import           Log                       (Message, MessageLog)

npcAction :: Entity -> State Dungeon MessageLog
npcAction e = do
        u <- updatePathOrMelee e

        case u of
            Right e' -> moveOrWait e'
            Left m   -> return m

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
                         d' <- get
                         let newPath = getPathTo d' pos (p ^. position)
                             newEntity = e & pathToDestination .~ fromMaybe [] newPath

                         return $ Right newEntity
            else return $ Right e

moveOrWait :: Entity -> State Dungeon MessageLog
moveOrWait e =
        let p = e ^. pathToDestination
        in if null p
            then do
                    waitAction e
                    return []
            else let (nextCoord, remaining) = (head p, tail p)
                     offset = nextCoord - e ^. position
                     newEntity = e & pathToDestination .~ remaining
                 in do
                     moveAction newEntity offset
                     return []
