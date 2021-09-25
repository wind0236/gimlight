module Actions
    ( bumpAction
    , meleeAction
    ) where

import           Control.Lens              (use, (&), (.~), (^.))
import           Control.Monad.Trans.State (State, execState, runState, state)
import           Coord                     (Coord)
import           Data.Array                ((!))
import           Data.List                 (find)
import           Data.Maybe                (isJust, isNothing)
import           Dungeon                   (Dungeon, enemies, entities,
                                            pushEntity, tileMap)
import           Dungeon.Map.Tile          (walkable)
import qualified Dungeon.Size              as DS
import           Entity                    (Entity, name, position)
import           Linear.V2                 (V2 (..), _x, _y)
import           Log                       (Message, attackMessage)

bumpAction :: Entity -> V2 Int -> State Dungeon (Maybe Message)
bumpAction src offset = state $ \d ->
    if isJust $ getBlockingEntityAtLocation (src ^. position + offset) d
        then runState (meleeAction src offset) d
        else
            (Nothing, execState (moveAction src offset) d)

getBlockingEntityAtLocation :: Coord -> Dungeon -> Maybe Entity
getBlockingEntityAtLocation c d =
        find (\x -> (x ^. position) == c) (enemies d)

meleeAction :: Entity -> V2 Int -> State Dungeon (Maybe Message)
meleeAction src offset = do
        es <- use entities

        let pos = src ^. position
            dest = pos + offset
            entity = find (\x -> x ^. position == dest) es
            entityName = fmap (\x -> "Hello, " ++ x ^. name) entity

        pushEntity src
        return $ fmap attackMessage entityName

moveAction :: Entity -> V2 Int -> State Dungeon ()
moveAction src offset = state $ \d -> ((), execState (pushEntity $ updatePosition src offset d) d)

updatePosition :: Entity -> V2 Int -> Dungeon -> Entity
updatePosition src offset g
    = let next = nextPosition src offset
      in if movable next g
            then src & position .~ next
            else src

nextPosition :: Entity -> V2 Int -> Coord
nextPosition src offset =
    max (V2 0 0) $ min (V2 (DS.width - 1) $ DS.height - 1) $ (src ^. position) + offset

movable :: Coord -> Dungeon -> Bool
movable c d
    = ((d ^. tileMap) ! (c ^. _x, c ^. _y) ^. walkable) && isNothing (getBlockingEntityAtLocation c d)
