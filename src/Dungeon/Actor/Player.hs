module Dungeon.Actor.Player
    ( playerBumpAction
    , handlePlayerMoving
    , handlePlayerPickingUp
    , handlePlayerSelectingItemToUse
    , handlePlayerConsumeItem
    ) where

import           Control.Lens                   ((^.))
import           Data.Bifunctor                 (Bifunctor (second))
import           Dungeon                        (isTown)
import           Dungeon.Actor                  (Actor, isMonster, talkMessage)
import qualified Dungeon.Actor                  as A
import           Dungeon.Actor.Actions.Consume  (consumeAction)
import           Dungeon.Actor.Actions.Melee    (meleeAction)
import           Dungeon.Actor.Actions.Move     (moveAction)
import           Dungeon.Actor.Actions.PickUp   (pickUpAction)
import           Game.Status                    (GameStatus (Exploring, GameOver, SelectingItemToUse, Talking))
import           Game.Status.Exploring          (ExploringHandler, actorAt,
                                                 completeThisTurn,
                                                 doPlayerAction,
                                                 getCurrentDungeon,
                                                 getPlayerActor,
                                                 getPlayerPosition,
                                                 isPositionInDungeon)
import qualified Game.Status.Exploring          as GSE
import           Game.Status.SelectingItemToUse (SelectingItemToUseHandler,
                                                 finishSelecting,
                                                 getSelectingIndex,
                                                 selectingItemToUseHandler)
import           Game.Status.Talking            (talkingHandler)
import           Linear.V2                      (V2)
import           Talking                        (talkWith)

playerBumpAction :: V2 Int -> ExploringHandler -> (Bool, GameStatus)
playerBumpAction offset eh =
    let destination = case getPlayerPosition eh of
                          Just p  -> p + offset
                          Nothing -> error "The player is dead."
        action = case actorAt destination eh of
                     Just actorAtDestination -> meleeOrTalk offset actorAtDestination
                     Nothing -> moveOrExitMap offset
    in action eh

meleeOrTalk :: V2 Int -> Actor -> ExploringHandler -> (Bool, GameStatus)
meleeOrTalk offset target eh =
    if isMonster target
        then second Exploring $ doPlayerAction (meleeAction offset) eh
        else (True, Talking $ talkingHandler (talkWith target $ target ^. talkMessage) eh)

moveOrExitMap :: V2 Int -> ExploringHandler -> (Bool, GameStatus)
moveOrExitMap offset eh =
    let destination = case getPlayerPosition eh of
                          Just p  -> p + offset
                          Nothing -> error "The player is dead."
    in if isPositionInDungeon destination eh || not (isTown (getCurrentDungeon eh))
        then second Exploring $ doPlayerAction (moveAction offset) eh
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
                 Exploring eh -> maybe GameOver Exploring (completeThisTurn eh)
                 _            -> newState
        else newState

handlePlayerPickingUp :: ExploringHandler -> GameStatus
handlePlayerPickingUp eh =
    let (isSuccess, newHandler) = doPlayerAction pickUpAction eh
    in if isSuccess
        then maybe GameOver Exploring $ completeThisTurn newHandler
        else Exploring newHandler

handlePlayerSelectingItemToUse :: ExploringHandler -> GameStatus
handlePlayerSelectingItemToUse eh =
    SelectingItemToUse $ selectingItemToUseHandler xs eh
    where xs = A.getItems p
          p = case getPlayerActor eh of
                Just x  -> x
                Nothing -> error "Player is dead."

handlePlayerConsumeItem :: SelectingItemToUseHandler -> GameStatus
handlePlayerConsumeItem sh =
    case getSelectingIndex sh of
        Just n ->
            let (isSuccess, newHandler) = doPlayerAction (consumeAction n) $ finishSelecting sh
            in if isSuccess
                then maybe GameOver Exploring (completeThisTurn newHandler)
                else Exploring newHandler
        Nothing -> SelectingItemToUse sh
