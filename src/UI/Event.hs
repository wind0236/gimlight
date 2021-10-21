{-# LANGUAGE OverloadedStrings #-}

module UI.Event
    ( handleEvent
    ) where

import           Data.Text   (Text)
import           Game        (Game, finishSelecting, finishTalking,
                              handlePlayerConsumingItem,
                              handlePlayerEnteringTown, handlePlayerMoving,
                              handlePlayerPickingUp,
                              handlePlayerSelectingItemToUse, isHandlingScene,
                              isPlayerExploring, isPlayerTalking,
                              isSelectingItemToUse, isSelectingLocale, isTitle,
                              loadStatus, nextSceneElementOrFinish, saveStatus,
                              selectNextItem, selectPrevItem, setLocale,
                              startNewGame)
import           Game.Config (Language (English, Japanese))
import           Linear.V2   (V2 (V2))
import           Monomer     (AppEventResponse, EventResponse (Model, Task),
                              WidgetEnv, WidgetNode, exitApplication)
import           UI.Types    (AppEvent (AppInit, AppKeyboardInput, AppLoadFinished, AppSaveFinished))

handleEvent :: WidgetEnv Game AppEvent -> WidgetNode Game AppEvent -> Game -> AppEvent -> [AppEventResponse Game AppEvent]
handleEvent _ _ gameStatus evt =
    case evt of
        AppInit             -> []
        AppSaveFinished     -> []
        AppLoadFinished ngs -> [Model ngs]
        AppKeyboardInput k  -> handleKeyInput gameStatus k

handleKeyInput :: Game -> Text -> [AppEventResponse Game AppEvent]
handleKeyInput e k
    | isPlayerExploring e = handleKeyInputDuringExploring e k
    | isPlayerTalking e = handleKeyInputDuringTalking e k
    | isHandlingScene e = handleKeyInputDuringHandlingScene e k
    | isSelectingItemToUse e = handleKeyInputDuringSelectingItemToUse e k
    | isTitle e = handleKeyInputDuringTitle e k
    | isSelectingLocale e = handleKeyInputDuringSelectingLanguage e k
    | otherwise = undefined

handleKeyInputDuringExploring :: Game -> Text -> [AppEventResponse Game AppEvent]
handleKeyInputDuringExploring e k
    | k == "Right" = [Model $ handlePlayerMoving (V2 1 0) e]
    | k == "Left"  = [Model $ handlePlayerMoving (V2 (-1) 0) e]
    | k == "Up"    = [Model $ handlePlayerMoving (V2 0 1) e]
    | k == "Down"  = [Model $ handlePlayerMoving (V2 0 (-1)) e]
    | k == "g" = [Model $ handlePlayerPickingUp e]
    | k == "u" = [Model $ handlePlayerSelectingItemToUse e]
    | k == "Ctrl-s"     = [Task (saveStatus e >> return AppSaveFinished)]
    | k == "Ctrl-l"     = [Task $ AppLoadFinished <$> loadStatus e]
    | k == "Enter" = [Model $ handlePlayerEnteringTown e]
    | otherwise = []

handleKeyInputDuringTalking :: Game -> Text -> [AppEventResponse Game AppEvent]
handleKeyInputDuringTalking e k
    | k == "Enter" = [Model $ finishTalking e]
    | otherwise = []

handleKeyInputDuringHandlingScene :: Game -> Text -> [AppEventResponse Game AppEvent]
handleKeyInputDuringHandlingScene e k
    | k == "Enter" = [Model $ nextSceneElementOrFinish e]
    | otherwise = []

handleKeyInputDuringSelectingItemToUse :: Game -> Text -> [AppEventResponse Game AppEvent]
handleKeyInputDuringSelectingItemToUse e k
    | k == "Up" = [Model $ selectPrevItem e]
    | k == "Down" = [Model $ selectNextItem e]
    | k == "Enter" = [Model $ handlePlayerConsumingItem e]
    | k == "Esc" = [Model $ finishSelecting e]
    | otherwise = []

handleKeyInputDuringTitle :: Game -> Text -> [AppEventResponse Game AppEvent]
handleKeyInputDuringTitle g k
    | k == "n" = [Task $ AppLoadFinished <$> startNewGame g]
    | k == "l" = [Task $ AppLoadFinished <$> loadStatus g]
    | k == "q" = [exitApplication]
    | otherwise = []

handleKeyInputDuringSelectingLanguage :: Game -> Text -> [AppEventResponse Game AppEvent]
handleKeyInputDuringSelectingLanguage g k
    | k == "e" = [Task $ AppLoadFinished <$> setLocale English g]
    | k == "j" = [Task $ AppLoadFinished <$> setLocale Japanese g]
    | otherwise = []
