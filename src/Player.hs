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
import           Actor                    (Actor, getTalkingPart, isMonster,
                                           position)
import qualified Actor                    as A
import           Control.Lens             ((^.))
import           Data.Foldable            (find)
import           Data.Maybe               (fromMaybe)
import           Dungeon                  (getActors, isTown)
import           GameStatus               (GameStatus (Exploring, GameOver, ReadingBook, SelectingItem, Talking))
import           GameStatus.Exploring     (ExploringHandler, doPlayerAction,
                                           getCurrentDungeon, getPlayerActor,
                                           getPlayerPosition,
                                           isPositionInDungeon,
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
handlePlayerMoving offset gs =
    let (isSuccess, newState) = playerBumpAction offset gs
     in if isSuccess
            then case newState of
                     Exploring eh ->
                         maybe GameOver Exploring (processAfterPlayerTurn eh)
                     _ -> newState
            else newState

handlePlayerPickingUp :: ExploringHandler -> GameStatus
handlePlayerPickingUp eh =
    let (status, newHandler) = doPlayerAction pickUpAction eh
     in case status of
            Ok -> maybe GameOver Exploring $ processAfterPlayerTurn newHandler
            ReadingStarted _ -> error "Unreachable."
            Failed -> Exploring newHandler

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
playerBumpAction offset eh =
    let action =
            case actorAtDestination of
                Just x  -> meleeOrTalk offset x
                Nothing -> moveOrExitMap offset
     in action eh
  where
    actorAtDestination =
        find (\x -> x ^. position == destination) $
        getActors $ getCurrentDungeon eh
    destination =
        case getPlayerPosition eh of
            Just p  -> p + offset
            Nothing -> error "The player is dead."

meleeOrTalk :: V2 Int -> Actor -> ExploringHandler -> (Bool, GameStatus)
meleeOrTalk offset target eh =
    if isMonster target
        then let (status, newHandler) = doPlayerAction (meleeAction offset) eh
              in case status of
                     Ok               -> (True, Exploring newHandler)
                     ReadingStarted _ -> error "Unreachable."
                     Failed           -> (False, Exploring newHandler)
        else case getTalkingPart target of
                 Just x  -> (True, Talking $ talkingHandler target x eh)
                 Nothing -> error "No talk handler is set."

moveOrExitMap :: V2 Int -> ExploringHandler -> (Bool, GameStatus)
moveOrExitMap offset eh =
    let destination =
            case getPlayerPosition eh of
                Just p  -> p + offset
                Nothing -> error "The player is dead."
     in if isPositionInDungeon destination eh ||
           not (isTown (getCurrentDungeon eh))
            then let (status, newHandler) =
                         doPlayerAction (moveAction offset) eh
                  in case status of
                         Ok               -> (True, Exploring newHandler)
                         ReadingStarted _ -> error "Unreachable."
                         Failed           -> (False, Exploring newHandler)
            else (True, exitDungeon eh)

exitDungeon :: ExploringHandler -> GameStatus
exitDungeon eh =
    case GSE.exitDungeon eh of
        Just newEh -> Exploring newEh
        Nothing    -> error "Failed to exit from the dungeon."
