module Actor.NpcBehavior
    ( handleNpcTurns
    ) where

import           Action               (Action, ActionResult (killed),
                                       newDungeon)
import           Action.Melee         (meleeAction)
import           Action.Move          (moveAction)
import           Action.Wait          (waitAction)
import           Actor                (Actor, getIndex, isFriendlyNpc,
                                       pathToDestination, position, target)
import           Control.Arrow        ((&&&))
import           Control.Lens         ((&), (.~), (^.))
import           Control.Monad.Writer (MonadWriter (writer), Writer)
import           Coord                (Coord)
import           Data.Array           ((!))
import           Data.Foldable        (find)
import           Data.Maybe           (fromMaybe)
import           Dungeon              (Dungeon, calculateFovAt, getActors, npcs,
                                       popActorAt, pushActor)
import           Dungeon.Map.Tile     (TileCollection)
import           Dungeon.PathFinder   (getPathTo)
import           Linear.V2            (V2 (V2))
import           Log                  (MessageLog)

handleNpcTurns ::
       TileCollection -> Dungeon -> Writer MessageLog (Dungeon, [Actor])
handleNpcTurns ts d = foldl foldStep (writer ((d, []), [])) $ npcs d
  where
    foldStep acc x = do
        (d', l) <- acc
        (newD, newLog) <- handleNpcTurn (x ^. position) ts d'
        return (newD, newLog ++ l)

handleNpcTurn ::
       Coord
    -> TileCollection
    -> Dungeon
    -> Writer MessageLog (Dungeon, [Actor])
handleNpcTurn c ts d = maybe (return (d, [])) doAction theActor
  where
    (theActor, dungeonWithoutTheActor) = popActorAt c d
    doAction actor = npcAction actor ts dungeonWithoutTheActor

npcAction ::
       Actor
    -> TileCollection
    -> Dungeon
    -> Writer MessageLog (Dungeon, [Actor])
npcAction e ts d
    | isFriendlyNpc e = return (pushActor e d, [])
    | otherwise = (newDungeon &&& killed) <$> action entityAfterUpdatingMap ts d
  where
    action = selectAction entityAfterUpdatingMap d
    entityAfterUpdatingMap =
        fromMaybe
            entityAfterUpdatingTarget
            (updatePath entityAfterUpdatingTarget ts d)
    entityAfterUpdatingTarget = updateTarget ts d e

updateTarget :: TileCollection -> Dungeon -> Actor -> Actor
updateTarget ts d a
    | currentTargetIsInFov = a
    | otherwise = a & target .~ (getIndex <$> nextTarget)
  where
    currentTargetIsInFov =
        case currentTarget of
            Just x  -> fov ! (x ^. position)
            Nothing -> False
    currentTarget = find (\x -> a ^. target == Just (getIndex x)) $ getActors d
    nextTarget
        | null otherActorsInFov = Nothing
        | otherwise = Just $ head otherActorsInFov
    otherActorsInFov = filter (\x -> fov ! (x ^. position)) otherActors
    fov = calculateFovAt (a ^. position) ts d
    otherActors =
        filter (\x -> (x ^. position) /= (a ^. position)) $ getActors d

updatePath :: Actor -> TileCollection -> Dungeon -> Maybe Actor
updatePath e ts d
    | isTargetInFov ts d e = Just $ e & pathToDestination .~ newPath
    | otherwise = Nothing
  where
    newPath = fromMaybe [] $ dst >>= getPathTo ts d src
    src = e ^. position
    dst = getTargetPosition e d

selectAction :: Actor -> Dungeon -> Action
selectAction e d
    | targetIsNextTo e d =
        case offsetToTarget e d of
            Just offset -> meleeAction offset
            Nothing     -> moveOrWait e
    | otherwise = moveOrWait e

moveOrWait :: Actor -> Action
moveOrWait e
    | null $ e ^. pathToDestination = waitAction
    | otherwise = popPathToDestinationAndMove

popPathToDestinationAndMove :: Action
popPathToDestinationAndMove e = moveAction offset e'
  where
    offset = next - e ^. position
    (next, remaining) = (head path, tail path)
    path = e ^. pathToDestination
    e' = e & pathToDestination .~ remaining

targetIsNextTo :: Actor -> Dungeon -> Bool
targetIsNextTo e d =
    case distance of
        Just d' -> d' <= 1
        Nothing -> False
  where
    distance = (\(V2 x y) -> max (abs x) (abs y)) <$> offsetToTarget e d

offsetToTarget :: Actor -> Dungeon -> Maybe (V2 Int)
offsetToTarget e d = subtract (e ^. position) <$> getTargetPosition e d

isTargetInFov :: TileCollection -> Dungeon -> Actor -> Bool
isTargetInFov ts d actor =
    ((calculateFovAt (actor ^. position) ts d !) <$> getTargetPosition actor d) ==
    Just True

getTargetPosition :: Actor -> Dungeon -> Maybe Coord
getTargetPosition actor d =
    (^. position) <$>
    find (\other -> actor ^. target == Just (getIndex other)) (getActors d)
