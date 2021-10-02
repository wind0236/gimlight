module Actions
    ( bumpAction
    , meleeAction
    , waitAction
    , enemyAction
    ) where

import           Control.Lens              (use, (%~), (&), (.=), (.~), (^.))
import           Control.Monad.Trans.State (State, evalState, execState, get,
                                            runState, state)
import           Coord                     (Coord)
import           Data.Array                ((!))
import           Data.List                 (find)
import           Data.Maybe                (fromMaybe, isJust, isNothing)
import           Dungeon                   (Dungeon, enemies, entities,
                                            getPlayerEntity, popActorAt,
                                            pushEntity, tileMap, visible)
import           Dungeon.PathFinder        (getPathTo)
import qualified Dungeon.Size              as DS
import           Entity                    (Ai (..), Entity, ai, blocksMovement,
                                            defence, getHp, isAlive, isPlayer,
                                            name, path, position, power,
                                            updateHp)
import           Linear.V2                 (V2 (..), _x, _y)
import           Log                       (Message, attackMessage)
import           Map.Tile                  (walkable)

enemyAction :: Entity -> State Dungeon [Message]
enemyAction e = do
        u <- updatePathOrMelee e

        case u of
            Right e -> do
                            m <- moveOrWait e
                            return $ case m of
                                         Nothing -> []
                                         Just x  -> [x]
            Left m  -> return m

updatePathOrMelee :: Entity -> State Dungeon (Either [Message] Entity)
updatePathOrMelee e = do
        d <- get

        let p = getPlayerEntity d
            pos = e ^. position
            posDiff = p ^. position - pos
            distance = max (abs posDiff ^. _x) (abs posDiff ^. _y)

        v <- use visible

        if v ! (pos ^. _x, pos ^. _y)
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


moveOrWait :: Entity -> State Dungeon (Maybe Message)
moveOrWait e =
        let p = e ^. ai . path
        in if null p
            then do
                    waitAction e
                    return Nothing
            else let (nextCoord, remaining) = (head p, tail p)
                     offset = nextCoord - e ^. position
                     newAi = HostileEnemy { _path = remaining }
                     newEntity = e & ai .~ newAi
                 in do
                     moveAction newEntity offset
                     return Nothing

bumpAction :: Entity -> V2 Int -> State Dungeon [Message]
bumpAction src offset = do
        d <- get

        let x = getAliveActorAtLocation d (src ^. position + offset)

        case x of
            Just _  -> meleeAction src offset
            Nothing -> do
                moveAction src offset
                return []

getBlockingEntityAtLocation :: Dungeon -> Coord -> Maybe Entity
getBlockingEntityAtLocation d c = find (\x -> x ^. position == c && x ^. blocksMovement) $ enemies d

getAliveActorAtLocation :: Dungeon -> Coord -> Maybe Entity
getAliveActorAtLocation d c = find (\x -> x ^. position == c && x ^. isAlive) $ enemies d

meleeAction :: Entity -> V2 Int -> State Dungeon [Message]
meleeAction src offset = do
        es <- use entities

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
            Just x -> let damage = (src ^. power) - (x ^. defence)
                          msg = (src ^. name) ++ " attacks " ++ (x ^. name)
                        in if damage > 0
                            then do
                                let newHp = getHp x - damage
                                    newEntity = updateHp x newHp
                                    damagedMessage = msg ++ " for " ++ show damage ++ " hit points."
                                    deathMessage = if x ^. isPlayer then "You died!" else (x ^. name) ++ " is dead!"
                                    messages = if newHp <= 0 then [damagedMessage, deathMessage] else [damagedMessage]
                                pushEntity src
                                pushEntity newEntity
                                return $ fmap attackMessage messages
                            else do
                                    pushEntity src
                                    pushEntity x
                                    return [attackMessage $ msg ++ " but does not damage."]

moveAction :: Entity -> V2 Int -> State Dungeon ()
moveAction src offset = state $ \d -> ((), execState (pushEntity $ updatePosition d src offset) d)

waitAction :: Entity -> State Dungeon ()
waitAction = pushEntity

updatePosition :: Dungeon -> Entity -> V2 Int -> Entity
updatePosition d src offset
    = let next = nextPosition src offset
      in if movable d next
            then src & position .~ next
            else src

nextPosition :: Entity -> V2 Int -> Coord
nextPosition src offset =
    max (V2 0 0) $ min (V2 (DS.width - 1) $ DS.height - 1) $ src ^. position + offset

movable :: Dungeon -> Coord -> Bool
movable d c = case getBlockingEntityAtLocation d c of
                  Just _  -> False
                  Nothing -> (d ^. tileMap) ! (c ^. _x, c ^. _y) ^. walkable
