{-# LANGUAGE OverloadedStrings #-}

module UI.Event
    ( handleEvent
    ) where

import           Control.Monad.Trans.State (execState)
import           Data.Text                 (Text)
import           Game                      (Game (Game, config, status))
import           Game.Config               (Language (English, Japanese),
                                            setLocale, writeConfig)
import           Game.Status               (enterTownAtPlayerPosition,
                                            finishSelecting, finishTalking,
                                            isHandlingScene, isPlayerExploring,
                                            isPlayerTalking,
                                            isSelectingItemToUse,
                                            isSelectingLocale, isTitle,
                                            newGameStatus,
                                            nextSceneElementOrFinish,
                                            selectNextItem, selectPrevItem,
                                            title)
import           Game.Status.Player        (handlePlayerConsumeItem,
                                            handlePlayerMoving,
                                            handlePlayerPickingUp,
                                            handlePlayerSelectingItemToUse)
import           Linear.V2                 (V2 (V2))
import           Monomer                   (AppEventResponse,
                                            EventResponse (Model, Task),
                                            WidgetEnv, WidgetNode,
                                            exitApplication)
import           Save                      (load, save)
import           UI.Types                  (AppEvent (AppInit, AppKeyboardInput, AppLoadFinished, AppSaveFinished))

handleEvent :: WidgetEnv Game AppEvent -> WidgetNode Game AppEvent -> Game -> AppEvent -> [AppEventResponse Game AppEvent]
handleEvent _ _ gameStatus evt =
    case evt of
        AppInit             -> []
        AppSaveFinished     -> []
        AppLoadFinished ngs -> [Model ngs]
        AppKeyboardInput k  -> handleKeyInput gameStatus k

handleKeyInput :: Game -> Text -> [AppEventResponse Game AppEvent]
handleKeyInput e@Game { status = s } k
    | isPlayerExploring s = handleKeyInputDuringExploring e k
    | isPlayerTalking s = handleKeyInputDuringTalking e k
    | isHandlingScene s = handleKeyInputDuringHandlingScene e k
    | isSelectingItemToUse s = handleKeyInputDuringSelectingItemToUse e k
    | isTitle s = handleKeyInputDuringTitle e k
    | isSelectingLocale s = handleKeyInputDuringSelectingLanguage e k
    | otherwise = undefined

handleKeyInputDuringExploring :: Game -> Text -> [AppEventResponse Game AppEvent]
handleKeyInputDuringExploring e@Game { status = st } k
    | k == "Right" = [Model $ e { status = execState (handlePlayerMoving (V2 1 0)) st }]
    | k == "Left"  = [Model $ e { status = execState (handlePlayerMoving (V2 (-1) 0)) st }]
    | k == "Up"    = [Model $ e { status = execState (handlePlayerMoving (V2 0 1)) st}]
    | k == "Down"  = [Model $ e { status = execState (handlePlayerMoving (V2 0 (-1))) st}]
    | k == "g" = [Model e { status = execState handlePlayerPickingUp st}]
    | k == "u" = [Model e { status = handlePlayerSelectingItemToUse st }]
    | k == "Ctrl-s"     = [Task (save st >> return AppSaveFinished)]
    | k == "Ctrl-l"     = [Task $ do
                            s <- load
                            return $ AppLoadFinished e { status = s }]
    | k == "Enter" = [Model e { status = enterTownAtPlayerPosition st }]
    | otherwise = []

handleKeyInputDuringTalking :: Game -> Text -> [AppEventResponse Game AppEvent]
handleKeyInputDuringTalking e@Game { status = s } k
    | k == "Enter" = [Model $ e { status = finishTalking s }]
    | otherwise = []

handleKeyInputDuringHandlingScene :: Game -> Text -> [AppEventResponse Game AppEvent]
handleKeyInputDuringHandlingScene e@Game { status = s } k
    | k == "Enter" = [Model $ e { status = nextSceneElementOrFinish s }]
    | otherwise = []

handleKeyInputDuringSelectingItemToUse :: Game -> Text -> [AppEventResponse Game AppEvent]
handleKeyInputDuringSelectingItemToUse e@Game { status = s } k
    | k == "Up" = [Model $ e { status = selectPrevItem s }]
    | k == "Down" = [Model $ e { status = selectNextItem s }]
    | k == "Enter" = [Model $ e { status = execState handlePlayerConsumeItem s }]
    | k == "Esc" = [Model $ e { status =  finishSelecting s }]
    | otherwise = []

handleKeyInputDuringTitle :: Game -> Text -> [AppEventResponse Game AppEvent]
handleKeyInputDuringTitle g k
    | k == "n" = [Task $ AppLoadFinished <$> startNewGame g]
    | k == "l" = [Task $ do
                    s <- load
                    return $ AppLoadFinished g { status = s }]
    | k == "q" = [exitApplication]
    | otherwise = []
    where startNewGame Game { config = c } =
            do
                st <- newGameStatus
                return Game { status = st
                            , config = c
                            }

handleKeyInputDuringSelectingLanguage :: Game -> Text -> [AppEventResponse Game AppEvent]
handleKeyInputDuringSelectingLanguage g@Game { config = c } k
    | k == "e" = [Task $ AppLoadFinished <$> updateConfig English]
    | k == "j" = [Task $ AppLoadFinished <$> updateConfig Japanese]
    | otherwise = []
    where updateConfig l = do
            let newConfig = setLocale l c
            writeConfig newConfig
            return $ g { status = title,
                    config = newConfig
                    }
