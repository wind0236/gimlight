module Actions
    ( bumpAction
    , meleeAction
    , waitAction
    , enemyAction
    ) where

import           Control.Lens              (use, (&), (.=), (.~), (^.))
import           Control.Monad.Trans.State (State, evalState, execState,
                                            runState, state)
import           Coord                     (Coord)
import           Data.Array                ((!))
import           Data.List                 (find)
import           Data.Maybe                (isJust, isNothing)
import           Dungeon                   (Dungeon, enemies, entities,
                                            getPlayerEntity, pushEntity,
                                            tileMap, visible)
import           Dungeon.Map.Tile          (walkable)
import           Dungeon.PathFinder        (getPathTo)
import qualified Dungeon.Size              as DS
import           Entity                    (Ai (..), Entity, ai, name, path,
                                            position)
import           Linear.V2                 (V2 (..), _x, _y)
import           Log                       (Message, attackMessage)

enemyAction :: Entity -> State Dungeon (Maybe Message)
enemyAction e = do
        p <- getPlayerEntity
        v <- use visible

        let posDiff = p ^. position - e ^. position
            distance = max (abs posDiff ^. _x) (abs posDiff ^. _y)

        u <- updatePathOrMelee e

        case u of
            Right e -> moveOrWait e
            Left m  -> return m

updatePathOrMelee :: Entity -> State Dungeon (Either (Maybe Message) Entity)
updatePathOrMelee e = do
        p <- getPlayerEntity
        v <- use visible

        let pos = e ^. position
            posDiff = (p ^. position) - pos
            distance = max (abs posDiff ^. _x) (abs posDiff ^. _y)

        if v ! (pos ^. _x, pos ^. _y)
            then if distance <= 1
                     then do
                         msg <- meleeAction e posDiff
                         return $ Left msg
                     else do
                         newPath <- getPathTo pos (p ^. position)

                         let    newAi = HostileEnemy { _path = newPath }
                                newEntity = e & ai .~ newAi
                         undefined
                         return $ Right newEntity

            else return $ Left Nothing


moveOrWait :: Entity -> State Dungeon (Maybe Message)
moveOrWait e =
        let p = e ^. (ai . path)

        in if null p
            then do
                    waitAction e
                    return Nothing
            else let (nextCoord, remaining) = (head p, tail p)
                     offset = nextCoord - e ^. position
                     newAi = HostileEnemy { _path = remaining }
                     newEntity = e & ai .~ newAi
                 in moveAction newEntity offset

bumpAction :: Entity -> V2 Int -> State Dungeon (Maybe Message)
bumpAction src offset = do
        x <- getBlockingEntityAtLocation (src ^. position + offset)

        case x of
            Just _  -> meleeAction src offset
            Nothing -> moveAction src offset

getBlockingEntityAtLocation :: Coord -> State Dungeon (Maybe Entity)
getBlockingEntityAtLocation c = find (\x -> x ^. position == c) <$> enemies

meleeAction :: Entity -> V2 Int -> State Dungeon (Maybe Message)
meleeAction src offset = do
        es <- use entities

        let pos = src ^. position
            dest = pos + offset
            entity = find (\x -> x ^. position == dest) es
            entityName = fmap (\x -> "Hello, " ++ x ^. name) entity

        pushEntity src
        return $ fmap attackMessage entityName

moveAction :: Entity -> V2 Int -> State Dungeon (Maybe Message)
moveAction src offset = state $ \d -> (Nothing, execState (pushEntity $ updatePosition src offset d) d)

waitAction :: Entity -> State Dungeon ()
waitAction = pushEntity

updatePosition :: Entity -> V2 Int -> Dungeon -> Entity
updatePosition src offset g
    = let next = nextPosition src offset
      in if evalState (movable next) g
            then src & position .~ next
            else src

nextPosition :: Entity -> V2 Int -> Coord
nextPosition src offset =
    max (V2 0 0) $ min (V2 (DS.width - 1) $ DS.height - 1) $ src ^. position + offset

movable :: Coord -> State Dungeon Bool
movable c = do
        t <- use tileMap
        e <- getBlockingEntityAtLocation c
        return $ case e of
            Just _  -> False
            Nothing -> t ! (c ^. _x, c ^. _y) ^. walkable
