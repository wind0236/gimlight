module Actor.NpcBehavior
    ( handleNpcTurns
    ) where

import           Action               (Action, ActionResult (killed),
                                       newDungeon)
import           Action.Melee         (meleeAction)
import           Action.Move          (moveAction)
import           Action.Wait          (waitAction)
import           Actor                (Actor, isFriendlyNpc, pathToDestination,
                                       position)
import           Control.Arrow        ((&&&))
import           Control.Lens         ((&), (.~), (^.))
import           Control.Monad.Writer (MonadWriter (writer), Writer)
import           Coord                (Coord)
import           Data.Maybe           (fromMaybe)
import           Dungeon              (Dungeon, getPlayerActor, npcs,
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
    | otherwise =
        (newDungeon &&& killed) <$> action entityAfterUpdatingPath ts d
  where
    entityAfterUpdatingPath = updatePath e ts d
    action = selectAction entityAfterUpdatingPath d

updatePath :: Actor -> TileCollection -> Dungeon -> Actor
updatePath e ts d = e & pathToDestination .~ newPath
  where
    newPath = fromMaybe [] $ dst >>= getPathTo ts d src
    src = e ^. position
    dst = fmap (^. position) player
    player = getPlayerActor d

selectAction :: Actor -> Dungeon -> Action
selectAction e d
    | isActorNextToPlayer e d =
        case offsetToPlayer e d of
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

isActorNextToPlayer :: Actor -> Dungeon -> Bool
isActorNextToPlayer e d =
    case distance of
        Just d' -> d' <= 1
        Nothing -> False
  where
    distance = (\(V2 x y) -> max (abs x) (abs y)) <$> offsetToPlayer e d

offsetToPlayer :: Actor -> Dungeon -> Maybe (V2 Int)
offsetToPlayer e d = fmap (\x -> x - e ^. position) playerPos
  where
    p = getPlayerActor d
    playerPos = fmap (^. position) p
