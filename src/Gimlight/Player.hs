module Gimlight.Player
    ( playerBumpAction
    , handlePlayerMoving
    , handlePlayerPickingUp
    , handlePlayerSelectingItem
    , handlePlayerAfterSelecting
    ) where

import           Control.Lens                      ((^.))
import           Data.Foldable                     (find)
import           Gimlight.Action                   (ActionStatus (Failed, Ok, ReadingStarted))
import           Gimlight.Action.Consume           (consumeAction)
import           Gimlight.Action.Drop              (dropAction)
import           Gimlight.Action.Melee             (meleeAction)
import           Gimlight.Action.Move              (moveAction)
import           Gimlight.Action.PickUp            (pickUpAction)
import           Gimlight.Actor                    (Actor, getTalkingPart,
                                                    isMonster)
import qualified Gimlight.Actor                    as A
import           Gimlight.Data.Maybe               (expectJust)
import           Gimlight.Dungeon                  (cellMap, getIdentifier)
import           Gimlight.Dungeon.Identifier       (isTown)
import           Gimlight.Dungeon.Map.Cell         (isPositionInMap,
                                                    positionsAndActors)
import           Gimlight.GameStatus               (GameStatus (Exploring, GameOver, ReadingBook, SelectingItem, Talking))
import           Gimlight.GameStatus.Exploring     (ExploringHandler,
                                                    doPlayerAction,
                                                    getCurrentDungeon,
                                                    getPlayerActor,
                                                    getPlayerPosition,
                                                    processAfterPlayerTurn)
import qualified Gimlight.GameStatus.Exploring     as GSE
import           Gimlight.GameStatus.ReadingBook   (readingBookHandler)
import           Gimlight.GameStatus.SelectingItem (Reason (Drop, Use),
                                                    SelectingItemHandler,
                                                    getExploringHandler,
                                                    getReason,
                                                    getSelectingIndex,
                                                    selectingItemHandler)
import           Gimlight.GameStatus.Talking       (talkingHandler)
import           Linear.V2                         (V2)

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
    p = expectJust "Player is dead." (getPlayerActor eh)

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
          not (isTown . getIdentifier $ getCurrentDungeon eh) =
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
