module Player
    ( playerBumpAction
    , handlePlayerMoving
    , handlePlayerPickingUp
    , handlePlayerSelectingItem
    , handlePlayerAfterSelecting
    ) where

import           Action                   (ActionStatus (Failed, Ok, ReadingStarted))
import           Action.Consume           (consumeAction)
import           Action.Drop              (dropAction)
import           Action.Melee             (meleeAction)
import           Action.Move              (moveAction)
import           Action.PickUp            (pickUpAction)
import           Actor                    (Actor, getTalkingPart, isMonster)
import qualified Actor                    as A
import           Control.Lens             ((^.))
import           Data.Foldable            (find)
import           Data.Maybe               (fromMaybe)
import           Dungeon                  (cellMap, isTown)
import           Dungeon.Map.Cell         (isPositionInMap, positionsAndActors)
import           GameStatus               (GameStatus (Exploring, GameOver, ReadingBook, SelectingItem, Talking))
import           GameStatus.Exploring     (ExploringHandler, doPlayerAction,
                                           getCurrentDungeon, getPlayerActor,
                                           getPlayerPosition,
                                           processAfterPlayerTurn)
import qualified GameStatus.Exploring     as GSE
import           GameStatus.ReadingBook   (readingBookHandler)
import           GameStatus.SelectingItem (Reason (Drop, Use),
                                           SelectingItemHandler,
                                           getExploringHandler, getReason,
                                           getSelectingIndex,
                                           selectingItemHandler)
import           GameStatus.Talking       (talkingHandler)
import           Linear.V2                (V2)

handlePlayerMoving :: V2 Int -> ExploringHandler -> GameStatus
handlePlayerMoving offset gs
    | isSuccess =
        case newState of
            Exploring eh -> maybe GameOver Exploring (processAfterPlayerTurn eh)
            _            -> newState
    | otherwise = newState
  where
    (isSuccess, newState) = playerBumpAction offset gs

handlePlayerPickingUp :: ExploringHandler -> GameStatus
handlePlayerPickingUp eh =
    case status of
        Ok -> maybe GameOver Exploring $ processAfterPlayerTurn newHandler
        ReadingStarted _ -> error "Unreachable."
        Failed -> Exploring newHandler
  where
    (status, newHandler) = doPlayerAction pickUpAction eh

handlePlayerSelectingItem :: Reason -> ExploringHandler -> GameStatus
handlePlayerSelectingItem reason eh =
    SelectingItem $ selectingItemHandler xs reason eh
  where
    xs = A.getItems p
    p = fromMaybe (error "Player is dead.") (getPlayerActor eh)

handlePlayerAfterSelecting :: SelectingItemHandler -> GameStatus
handlePlayerAfterSelecting h = result
  where
    result =
        case getSelectingIndex h of
            Just n  -> uncurry newState $ actionResult n
            Nothing -> SelectingItem h
    newState status handlerAfterAction =
        case status of
            Ok ->
                maybe GameOver Exploring $
                processAfterPlayerTurn handlerAfterAction
            ReadingStarted book ->
                ReadingBook $ readingBookHandler book handlerAfterAction
            Failed -> Exploring handlerAfterAction
    actionResult index = doPlayerAction (action index) $ getExploringHandler h
    action =
        case getReason h of
            Use  -> consumeAction
            Drop -> dropAction

playerBumpAction :: V2 Int -> ExploringHandler -> (Bool, GameStatus)
playerBumpAction offset eh = action eh
  where
    action =
        case actorAtDestination of
            Just x  -> meleeOrTalk offset x
            Nothing -> moveOrExitMap offset
    actorAtDestination =
        snd <$>
        find
            (\(x, _) -> x == destination)
            (positionsAndActors $ getCurrentDungeon eh ^. cellMap)
    destination =
        case getPlayerPosition eh of
            Just p  -> p + offset
            Nothing -> error "The player is dead."

meleeOrTalk :: V2 Int -> Actor -> ExploringHandler -> (Bool, GameStatus)
meleeOrTalk offset target eh
    | isMonster target =
        case status of
            Ok               -> (True, Exploring newHandler)
            ReadingStarted _ -> error "Unreachable."
            Failed           -> (False, Exploring newHandler)
    | otherwise =
        case getTalkingPart target of
            Just x  -> (True, Talking $ talkingHandler target x eh)
            Nothing -> error "No talk handler is set."
  where
    (status, newHandler) = doPlayerAction (meleeAction offset) eh

moveOrExitMap :: V2 Int -> ExploringHandler -> (Bool, GameStatus)
moveOrExitMap offset eh
    | isPositionInMap destination (getCurrentDungeon eh ^. cellMap) ||
          not (isTown (getCurrentDungeon eh)) =
        case status of
            Ok               -> (True, Exploring newHandler)
            ReadingStarted _ -> error "Unreachable."
            Failed           -> (False, Exploring newHandler)
    | otherwise = (True, exitDungeon eh)
  where
    destination =
        case getPlayerPosition eh of
            Just p  -> p + offset
            Nothing -> error "The player is dead."
    (status, newHandler) = doPlayerAction (moveAction offset) eh

exitDungeon :: ExploringHandler -> GameStatus
exitDungeon eh =
    case GSE.exitDungeon eh of
        Just newEh -> Exploring newEh
        Nothing    -> error "Failed to exit from the dungeon."
