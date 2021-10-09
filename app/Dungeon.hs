-- I refer to "Dungeon" in this source code as the mixed things of map and
-- entities because I could not come up with a much more proper word.  So,
-- in this code, "Dungeon" means not only dungeon but also towns, etc.
--
-- TODO: Change the word to more precise one.

module Dungeon
    ( initDungeon
    , Dungeon
    , dungeon
    , completeThisTurn
    , popPlayer
    , popActorAt
    , enemies
    , pushEntity
    , walkableFloor
    , getPlayerEntity
    , enemyCoords
    , aliveEnemies
    , mapWidthAndHeight
    , playerPosition
    ) where

import           Control.Lens                   ((%~), (&), (.=), (.~), (^.))
import           Control.Lens.Getter            (use)
import           Control.Monad.Trans.State      (State, runState, state)
import           Control.Monad.Trans.State.Lazy (execState, get)
import           Coord                          (Coord)
import           Data.Array.Base                (IArray (bounds))
import           Data.Foldable                  (find)
import           Data.List                      (findIndex)
import           Dungeon.Entity                 (Entity)
import qualified Dungeon.Entity                 as E
import           Dungeon.Map.Bool               (BoolMap)
import           Dungeon.Map.Explored           (updateExploredMap)
import           Dungeon.Map.Fov                (calculateFov)
import           Dungeon.Map.Tile               (transparent, walkable)
import           Dungeon.Predefined             (firstEventMap)
import qualified Dungeon.Turn                   as DT
import           Dungeon.Types                  (Dungeon, dungeon, entities,
                                                 explored, isAlive, isEnemy,
                                                 isPlayer, position, tileMap,
                                                 visible)
import           Linear.V2                      (V2 (..))

completeThisTurn :: State Dungeon DT.Status
completeThisTurn = do
        updateMap
        d <- get
        return $ if isPlayerAlive d then DT.Success else DT.PlayerKilled

updateMap :: State Dungeon ()
updateMap = do
        updateExplored
        updateFov

updateExplored :: State Dungeon ()
updateExplored = do
        v <- use visible
        e <- use explored

        explored .= updateExploredMap e v

updateFov :: State Dungeon ()
updateFov = do
        d <- get

        let t = transparentMap d
            p = getPlayerEntity d

        visible .= calculateFov (p ^. position) t

playerPosition :: Dungeon -> Coord
playerPosition d = getPlayerEntity d ^. position

getPlayerEntity :: Dungeon -> Entity
getPlayerEntity d =
        case find (^. isPlayer) $ d ^. entities of
            Just p  -> p
            Nothing -> error "No player entity."

pushEntity :: Entity -> State Dungeon ()
pushEntity e = state $ \d -> ((), d & entities %~ (e :))

popPlayer :: State Dungeon Entity
popPlayer = state $ \d -> case runState (popActorIf (^. isPlayer)) d of
                  (Just x, d') -> (x, d')
                  (Nothing, _) -> error "No player entity."

popActorAt :: Coord -> State Dungeon (Maybe Entity)
popActorAt c = popActorIf (\x -> x ^. position == c)

popActorIf :: (Entity -> Bool) -> State Dungeon (Maybe Entity)
popActorIf f = state $ \d ->
    let xs = d ^. entities
    in case findIndex f xs of
        Just x -> let entity = xs !! x
                      newEntities = take x xs ++ drop (x + 1) xs
                  in (Just entity, d & entities .~ newEntities)
        Nothing -> (Nothing, d)

walkableFloor :: Dungeon -> BoolMap
walkableFloor d = fmap (^. walkable) (d ^. tileMap)

transparentMap :: Dungeon -> BoolMap
transparentMap d = fmap (^. transparent) (d ^. tileMap)

enemyCoords :: Dungeon -> [Coord]
enemyCoords d = map (^. position) $ filter (not . (^. isPlayer)) $ d ^. entities

isPlayerAlive :: Dungeon -> Bool
isPlayerAlive d = getPlayerEntity d ^. isAlive

aliveEnemies :: Dungeon -> [Entity]
aliveEnemies d = filter (^. isAlive) $ enemies d

enemies :: Dungeon -> [Entity]
enemies d = filter (^. isEnemy) $ d ^. entities

mapWidthAndHeight :: Dungeon -> V2 Int
mapWidthAndHeight d = snd (bounds $ d ^. tileMap) + V2 1 1

initDungeon :: Dungeon
initDungeon =
        let player = E.player $ V2 5 5
            d = firstEventMap player
        in execState updateMap d
