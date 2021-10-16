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
    , monsters
    , pushEntity
    , walkableFloor
    , getPlayerEntity
    , enemyCoords
    , mapWidthAndHeight
    , playerPosition
    , initialPlayerPositionCandidates
    , updateMap
    , isGlobalMap
    , isTown
    , actorAt
    , isPositionInDungeon
    , npcs
    ) where

import           Control.Lens                   ((%~), (&), (.=), (.~), (^.))
import           Control.Lens.Getter            (use)
import           Control.Monad.Trans.State      (State, runState, state)
import           Control.Monad.Trans.State.Lazy (execState, get)
import           Coord                          (Coord)
import           Data.Array.Base                (IArray (bounds), assocs)
import           Data.Foldable                  (find)
import           Data.List                      (findIndex)
import           Data.Maybe                     (isJust)
import           Dungeon.Entity                 (Entity, isActor, isMonster,
                                                 isPlayer)
import qualified Dungeon.Entity                 as E
import           Dungeon.Map.Bool               (BoolMap)
import           Dungeon.Map.Explored           (updateExploredMap)
import           Dungeon.Map.Fov                (calculateFov)
import           Dungeon.Map.Tile               (transparent, walkable)
import           Dungeon.Predefined.Beaeve      (beaeve)
import qualified Dungeon.Turn                   as DT
import           Dungeon.Types                  (Dungeon,
                                                 DungeonKind (GlobalMap, Town),
                                                 dungeon, dungeonKind, entities,
                                                 explored, position, tileMap,
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

    case p of
        Just p' -> visible .= calculateFov (p' ^. position) t
        Nothing -> return ()

playerPosition :: Dungeon -> Maybe Coord
playerPosition d = (^. position) <$> getPlayerEntity d

getPlayerEntity :: Dungeon -> Maybe Entity
getPlayerEntity d = find isPlayer $ d ^. entities

actorAt :: Coord -> Dungeon -> Maybe Entity
actorAt c d = find (\x -> x ^. position == c) $ actors d

pushEntity :: Entity -> State Dungeon ()
pushEntity e = state $ \d -> ((), d & entities %~ (e :))

popPlayer :: State Dungeon Entity
popPlayer = state $ \d -> case runState (popActorIf isPlayer) d of
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

initialPlayerPositionCandidates :: Dungeon -> [Coord]
initialPlayerPositionCandidates d = filter (\x -> x `notElem` map (^. position) (d ^. entities)) $
    map fst $ filter snd $ assocs $ walkableFloor d

walkableFloor :: Dungeon -> BoolMap
walkableFloor d = fmap (^. walkable) (d ^. tileMap)

transparentMap :: Dungeon -> BoolMap
transparentMap d = fmap (^. transparent) (d ^. tileMap)

enemyCoords :: Dungeon -> [Coord]
enemyCoords d = map (^. position) $ filter (not . isPlayer) $ d ^. entities

isPlayerAlive :: Dungeon -> Bool
isPlayerAlive d = isJust $ getPlayerEntity d

actors :: Dungeon -> [Entity]
actors d = filter isActor $ d ^. entities

npcs :: Dungeon -> [Entity]
npcs d = filter (not . isPlayer) $ d ^. entities

monsters :: Dungeon -> [Entity]
monsters d = filter isMonster $ d ^. entities

mapWidthAndHeight :: Dungeon -> V2 Int
mapWidthAndHeight d = snd (bounds $ d ^. tileMap) + V2 1 1

isGlobalMap :: Dungeon -> Bool
isGlobalMap d = (d ^. dungeonKind) == GlobalMap

isTown :: Dungeon -> Bool
isTown d = (d ^. dungeonKind) ==  Town

isPositionInDungeon :: Coord -> Dungeon -> Bool
isPositionInDungeon c d = x >= 0 && x < width && y >= 0 && y < height
    where V2 width height = mapWidthAndHeight d
          V2 x y = c

initDungeon :: Dungeon
initDungeon =
        let player = E.player $ V2 5 5
            d = beaeve player
        in execState updateMap d
