module Dungeon.Actor.Player
    ( playerBumpAction
    , handlePlayerMoving
    , handlePlayerPickingUp
    , handlePlayerSelectingItemToDrop
    , handlePlayerSelectingItemToUse
    , handlePlayerConsumeItem
    , handlePlayerDropItem
    ) where

import           Control.Lens                         ((^.))
import           Data.Maybe                           (fromMaybe)
import           Dungeon                              (isTown)
import           Dungeon.Actor                        (Actor, isMonster,
                                                       talkMessage)
import qualified Dungeon.Actor                        as A
import           Dungeon.Actor.Actions                (ActionStatus (Failed, Ok, ReadingStarted))
import           Dungeon.Actor.Actions.Consume        (consumeAction)
import           Dungeon.Actor.Actions.Drop           (dropAction)
import           Dungeon.Actor.Actions.Melee          (meleeAction)
import           Dungeon.Actor.Actions.Move           (moveAction)
import           Dungeon.Actor.Actions.PickUp         (pickUpAction)
import           GameModel.Status                     (GameStatus (Exploring, GameOver, ReadingBook, SelectingItemToDrop, SelectingItemToUse, Talking))
import           GameModel.Status.Exploring           (ExploringHandler,
                                                       actorAt,
                                                       completeThisTurn,
                                                       doPlayerAction,
                                                       getCurrentDungeon,
                                                       getPlayerActor,
                                                       getPlayerPosition,
                                                       isPositionInDungeon)
import qualified GameModel.Status.Exploring           as GSE
import           GameModel.Status.ReadingBook         (readingBookHandler)
import           GameModel.Status.SelectingItemToDrop (SelectingItemToDropHandler,
                                                       selectingItemToDropHandler)
import qualified GameModel.Status.SelectingItemToDrop as D
import           GameModel.Status.SelectingItemToUse  (SelectingItemToUseHandler,
                                                       selectingItemToUseHandler)
import qualified GameModel.Status.SelectingItemToUse  as U
import           GameModel.Status.Talking             (talkingHandler)
import           Linear.V2                            (V2)
import           Talking                              (talkWith)

playerBumpAction :: V2 Int -> ExploringHandler -> (Bool, GameStatus)
playerBumpAction offset eh =
    let destination =
            case getPlayerPosition eh of
                Just p  -> p + offset
                Nothing -> error "The player is dead."
        action =
            case actorAt destination eh of
                Just actorAtDestination -> meleeOrTalk offset actorAtDestination
                Nothing                 -> moveOrExitMap offset
     in action eh

meleeOrTalk :: V2 Int -> Actor -> ExploringHandler -> (Bool, GameStatus)
meleeOrTalk offset target eh =
    if isMonster target
        then let (status, newHandler) = doPlayerAction (meleeAction offset) eh
              in case status of
                     Ok               -> (True, Exploring newHandler)
                     ReadingStarted _ -> error "Unreachable."
                     Failed           -> (False, Exploring newHandler)
        else ( True
             , Talking $
               talkingHandler (talkWith target $ target ^. talkMessage) eh)

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

handlePlayerMoving :: V2 Int -> ExploringHandler -> GameStatus
handlePlayerMoving offset gs =
    let (isSuccess, newState) = playerBumpAction offset gs
     in if isSuccess
            then case newState of
                     Exploring eh ->
                         maybe GameOver Exploring (completeThisTurn eh)
                     _ -> newState
            else newState

handlePlayerPickingUp :: ExploringHandler -> GameStatus
handlePlayerPickingUp eh =
    let (status, newHandler) = doPlayerAction pickUpAction eh
     in case status of
            Ok -> maybe GameOver Exploring $ completeThisTurn newHandler
            ReadingStarted _ -> error "Unreachable."
            Failed -> Exploring newHandler

handlePlayerSelectingItemToDrop :: ExploringHandler -> GameStatus
handlePlayerSelectingItemToDrop eh =
    SelectingItemToDrop $ selectingItemToDropHandler xs eh
  where
    xs = A.getItems p
    p = fromMaybe (error "Player is dead.") (getPlayerActor eh)

handlePlayerSelectingItemToUse :: ExploringHandler -> GameStatus
handlePlayerSelectingItemToUse eh =
    SelectingItemToUse $ selectingItemToUseHandler xs eh
  where
    xs = A.getItems p
    p = fromMaybe (error "Player is dead.") (getPlayerActor eh)

handlePlayerConsumeItem :: SelectingItemToUseHandler -> GameStatus
handlePlayerConsumeItem sh =
    case U.getSelectingIndex sh of
        Just n ->
            let (status, newHandler) =
                    doPlayerAction (consumeAction n) $ U.finishSelecting sh
             in case status of
                    Ok -> maybe GameOver Exploring $ completeThisTurn newHandler
                    ReadingStarted book ->
                        ReadingBook $ readingBookHandler book newHandler
                    Failed -> Exploring newHandler
        Nothing -> SelectingItemToUse sh

handlePlayerDropItem :: SelectingItemToDropHandler -> GameStatus
handlePlayerDropItem sh =
    case D.getSelectingIndex sh of
        Just n ->
            let (status, newHandler) =
                    doPlayerAction (dropAction n) $ D.finishSelecting sh
             in case status of
                    Ok -> maybe GameOver Exploring $ completeThisTurn newHandler
                    ReadingStarted _ -> error "unreachable."
                    Failed -> Exploring newHandler
        Nothing -> SelectingItemToDrop sh
