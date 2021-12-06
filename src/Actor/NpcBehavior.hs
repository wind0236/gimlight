module Actor.NpcBehavior
    ( handleNpcTurns
    ) where

import           Action               (Action, ActionResult (killed),
                                       newCellMap)
import           Action.Melee         (meleeAction)
import           Action.Move          (moveAction)
import           Action.Wait          (waitAction)
import           Actor                (Actor, getIndex, isFriendlyNpc,
                                       pathToDestination, target)
import           Control.Arrow        ((&&&))
import           Control.Lens         ((&), (.~), (^.))
import           Control.Monad.Writer (MonadWriter (writer), Writer)
import           Coord                (Coord)
import           Data.Array           ((!))
import           Data.Foldable        (find)
import           Data.Maybe           (fromMaybe)
import           Dungeon              (Dungeon, calculateFovAt, cellMap,
                                       getPositionsAndActors, positionsAndNpcs)
import           Dungeon.Map.Cell     (locateActorAt, removeActorAt,
                                       removeActorIf)
import           Dungeon.Map.Tile     (TileCollection)
import           Dungeon.PathFinder   (getPathTo)
import           Linear.V2            (V2 (V2))
import           Log                  (MessageLog)

handleNpcTurns ::
       TileCollection -> Dungeon -> Writer MessageLog (Dungeon, [Actor])
handleNpcTurns ts d = foldl foldStep (writer ((d, []), [])) $ positionsAndNpcs d
  where
    foldStep acc (position, _) = do
        (d', l) <- acc
        (newD, newLog) <- npcAction position ts d'
        return (newD, newLog ++ l)

npcAction ::
       Coord
    -> TileCollection
    -> Dungeon
    -> Writer MessageLog (Dungeon, [Actor])
npcAction position ts d =
    case removeActorAt position (d ^. cellMap) of
        Just (actor, cellMapWithoutActor) ->
            npcActionFor actor cellMapWithoutActor
        Nothing -> return (d, [])
  where
    npcActionFor a cm
        | isFriendlyNpc a = return (d, [])
        | otherwise =
            ((\x -> d & cellMap .~ newCellMap x) &&& killed) <$>
            action a position ts (cellMapAfterUpdatingMap a cm)
    action a = selectAction position (entityAfterUpdatingMap a) d
    cellMapAfterUpdatingMap a cm =
        fromMaybe
            (error "Failed to locate an actor.")
            (locateActorAt (entityAfterUpdatingMap a) position cm)
    entityAfterUpdatingMap a =
        fromMaybe
            (entityAfterUpdatingTarget a)
            (updatePath position (entityAfterUpdatingTarget a) ts d)
    entityAfterUpdatingTarget = updateTarget position ts d

updateTarget :: Coord -> TileCollection -> Dungeon -> Actor -> Actor
updateTarget srcPosition ts d a
    | currentTargetIsInFov = a
    | otherwise = a & target .~ (getIndex <$> nextTarget)
  where
    currentTargetIsInFov = maybe False isInFov currentTarget
    indexToPosition x =
        fst <$>
        find ((getIndex x ==) . getIndex . snd) (getPositionsAndActors d)
    currentTarget =
        fst <$>
        removeActorIf (\x -> a ^. target == Just (getIndex x)) (d ^. cellMap)
    nextTarget
        | null otherActorsInFov = Nothing
        | otherwise = Just $ head otherActorsInFov
    otherActorsInFov = filter isInFov otherActors
    fov = calculateFovAt srcPosition ts d
    isInFov x = maybe False (fov !) (indexToPosition x)
    otherActors =
        map snd $ filter (\(x, _) -> x /= srcPosition) $ getPositionsAndActors d

updatePath :: Coord -> Actor -> TileCollection -> Dungeon -> Maybe Actor
updatePath src e ts d
    | isTargetInFov src ts d e = Just $ e & pathToDestination .~ newPath
    | otherwise = Nothing
  where
    newPath = fromMaybe [] $ dst >>= getPathTo ts d src
    dst = getTargetPosition e d

selectAction :: Coord -> Actor -> Dungeon -> Action
selectAction position e d
    | targetIsNextTo position e d =
        case offsetToTarget position e d of
            Just offset -> meleeAction offset
            Nothing     -> moveOrWait e
    | otherwise = moveOrWait e

moveOrWait :: Actor -> Action
moveOrWait e
    | null $ e ^. pathToDestination = waitAction
    | otherwise = popPathToDestinationAndMove

popPathToDestinationAndMove :: Action
popPathToDestinationAndMove position tc cm =
    case removeActorAt position cm of
        Just (a, ncm) ->
            let path = a ^. pathToDestination
                (next, remaining) = (head path, tail path)
                offset = next - position
                updatedActor = a & pathToDestination .~ remaining
             in moveAction
                    offset
                    position
                    tc
                    (fromMaybe
                         (error "Failed to locate an actor")
                         (locateActorAt updatedActor position ncm))
        Nothing -> error "unreachable."

targetIsNextTo :: Coord -> Actor -> Dungeon -> Bool
targetIsNextTo position e d =
    case distance of
        Just d' -> d' <= 1
        Nothing -> False
  where
    distance =
        (\(V2 x y) -> max (abs x) (abs y)) <$> offsetToTarget position e d

offsetToTarget :: Coord -> Actor -> Dungeon -> Maybe (V2 Int)
offsetToTarget position e d = subtract position <$> getTargetPosition e d

isTargetInFov :: Coord -> TileCollection -> Dungeon -> Actor -> Bool
isTargetInFov position ts d actor =
    ((calculateFovAt position ts d !) <$> getTargetPosition actor d) ==
    Just True

getTargetPosition :: Actor -> Dungeon -> Maybe Coord
getTargetPosition actor d =
    fst <$>
    find
        (\(_, other) -> actor ^. target == Just (getIndex other))
        (getPositionsAndActors d)
