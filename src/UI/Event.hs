{-# LANGUAGE OverloadedStrings #-}

module UI.Event
    ( handleEvent
    ) where

import           Control.Monad.Trans.State (execState)
import           Data.Text                 (Text)
import           GameStatus                (GameStatus,
                                            enterTownAtPlayerPosition,
                                            finishSelecting, finishTalking,
                                            isHandlingScene, isPlayerExploring,
                                            isPlayerTalking,
                                            isSelectingItemToUse, isTitle,
                                            newGameStatus,
                                            nextSceneElementOrFinish,
                                            selectNextItem, selectPrevItem)
import qualified GameStatus.Player         as GSP
import           Linear.V2                 (V2 (V2))
import           Monomer                   (AppEventResponse,
                                            EventResponse (Model, Task),
                                            WidgetEnv, WidgetNode,
                                            exitApplication)
import           Save                      (load, save)
import           UI.Types                  (AppEvent (AppInit, AppKeyboardInput, AppLoadFinished, AppSaveFinished))

handleEvent :: WidgetEnv GameStatus AppEvent -> WidgetNode GameStatus AppEvent -> GameStatus -> AppEvent -> [AppEventResponse GameStatus AppEvent]
handleEvent _ _ gameStatus evt = case evt of
                                AppInit            -> []
                                AppSaveFinished    -> []
                                AppLoadFinished ngs  -> [Model ngs]
                                AppKeyboardInput k -> handleKeyInput gameStatus k

handleKeyInput :: GameStatus -> Text -> [AppEventResponse GameStatus AppEvent]
handleKeyInput e k
    | isPlayerExploring e = handleKeyInputDuringExploring e k
    | isPlayerTalking e = handleKeyInputDuringTalking e k
    | isHandlingScene e = handleKeyInputDuringHandlingScene e k
    | isSelectingItemToUse e = handleKeyInputDuringSelectingItemToUse e k
    | isTitle e = handleKeyInputDuringTitle k
    | otherwise = undefined

handleKeyInputDuringExploring :: GameStatus -> Text -> [AppEventResponse GameStatus AppEvent]
handleKeyInputDuringExploring e k
    | k == "Right" = [Model $ handlePlayerMoving (V2 1 0) e]
    | k == "Left"  = [Model $ handlePlayerMoving (V2 (-1) 0) e]
    | k == "Up"    = [Model $ handlePlayerMoving (V2 0 1) e]
    | k == "Down"  = [Model $ handlePlayerMoving (V2 0 (-1)) e]
    | k == "g" = [Model $ handlePlayerPickingUp e]
    | k == "u" = [Model $ GSP.handlePlayerSelectingItemToUse e]
    | k == "Ctrl-s"     = [Task (save e >> return AppSaveFinished)]
    | k == "Ctrl-l"     = [Task $ AppLoadFinished <$> load]
    | k == "Enter" = [Model $ enterTownAtPlayerPosition e]
    | otherwise = []

handleKeyInputDuringTalking :: GameStatus -> Text -> [AppEventResponse GameStatus AppEvent]
handleKeyInputDuringTalking e k
    | k == "Enter" = [Model $ finishTalking e]
    | otherwise = []

handleKeyInputDuringHandlingScene :: GameStatus -> Text -> [AppEventResponse GameStatus AppEvent]
handleKeyInputDuringHandlingScene e k
    | k == "Enter" = [Model $ nextSceneElementOrFinish e]
    | otherwise = []

handleKeyInputDuringSelectingItemToUse :: GameStatus -> Text -> [AppEventResponse GameStatus AppEvent]
handleKeyInputDuringSelectingItemToUse e k
    | k == "Up" = [Model $ selectPrevItem e]
    | k == "Down" = [Model $ selectNextItem e]
    | k == "Enter" = [Model $ handlePlayerConsumeItem e]
    | k == "Esc" = [Model $ finishSelecting e]
    | otherwise = []

handleKeyInputDuringTitle :: Text -> [AppEventResponse GameStatus AppEvent]
handleKeyInputDuringTitle k
    | k == "n" = [Task $ AppLoadFinished <$> newGameStatus]
    | k == "l" = [Task $ AppLoadFinished <$> load]
    | k == "q" = [exitApplication]
    | otherwise = []

handlePlayerMoving :: V2 Int -> GameStatus -> GameStatus
handlePlayerMoving offset e = flip execState e $ GSP.handlePlayerMoving offset

handlePlayerPickingUp :: GameStatus -> GameStatus
handlePlayerPickingUp = execState GSP.handlePlayerPickingUp

handlePlayerConsumeItem :: GameStatus -> GameStatus
handlePlayerConsumeItem = execState GSP.handlePlayerConsumeItem
