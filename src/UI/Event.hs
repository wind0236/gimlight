{-# LANGUAGE OverloadedStrings #-}

module UI.Event
    ( handleEvent
    ) where

import           Data.Text                      (Text)
import           Game                           (Game (Game, config, status))
import           Game.Config                    (Language (English, Japanese),
                                                 setLocale, writeConfig)
import           Game.Status                    (GameStatus (Exploring, GameOver, HandlingScene, SelectingItemToUse, SelectingLocale, Talking, Title),
                                                 newGameStatus)
import           Game.Status.Exploring          (enterTownAtPlayerPosition)
import           Game.Status.Player             (handlePlayerConsumeItem,
                                                 handlePlayerMoving,
                                                 handlePlayerPickingUp,
                                                 handlePlayerSelectingItemToUse)
import           Game.Status.Scene              (nextSceneOrFinish)
import           Game.Status.SelectingItemToUse (finishSelecting,
                                                 selectNextItem, selectPrevItem)
import           Game.Status.Talking            (finishTalking)
import           Linear.V2                      (V2 (V2))
import           Monomer                        (AppEventResponse,
                                                 EventResponse (Model, Task),
                                                 WidgetEnv, WidgetNode,
                                                 exitApplication)
import           Save                           (load, save)
import           UI.Types                       (AppEvent (AppInit, AppKeyboardInput, AppLoadFinished, AppSaveFinished))

handleEvent :: WidgetEnv Game AppEvent -> WidgetNode Game AppEvent -> Game -> AppEvent -> [AppEventResponse Game AppEvent]
handleEvent _ _ gameStatus evt =
    case evt of
        AppInit             -> []
        AppSaveFinished     -> []
        AppLoadFinished ngs -> [Model ngs]
        AppKeyboardInput k  -> handleKeyInput gameStatus k

handleKeyInput :: Game -> Text -> [AppEventResponse Game AppEvent]
handleKeyInput e@Game { status = s } k =
    case s of
        Exploring _          -> handleKeyInputDuringExploring e k
        Talking _            -> handleKeyInputDuringTalking e k
        HandlingScene _      -> handleKeyInputDuringHandlingScene e k
        SelectingItemToUse _ -> handleKeyInputDuringSelectingItemToUse e k
        Title                -> handleKeyInputDuringTitle e k
        SelectingLocale      -> handleKeyInputDuringSelectingLanguage e k
        GameOver             -> []


handleKeyInputDuringExploring :: Game -> Text -> [AppEventResponse Game AppEvent]
handleKeyInputDuringExploring e@Game { status = st@(Exploring eh) } k
    | k == "Right" = [Model $ e { status = handlePlayerMoving (V2 1 0) eh }]
    | k == "Left"  = [Model $ e { status = handlePlayerMoving (V2 (-1) 0) eh }]
    | k == "Up"    = [Model $ e { status = handlePlayerMoving (V2 0 1) eh}]
    | k == "Down"  = [Model $ e { status = handlePlayerMoving (V2 0 (-1)) eh}]
    | k == "g" = [Model e { status = handlePlayerPickingUp eh }]
    | k == "u" = [Model e { status = handlePlayerSelectingItemToUse eh }]
    | k == "Ctrl-s"     = [Task (save st >> return AppSaveFinished)]
    | k == "Ctrl-l"     = [Task $ do
                            s <- load
                            return $ AppLoadFinished e { status = s }]
    | k == "Enter" = [Model e { status = Exploring $ enterTownAtPlayerPosition eh }]
    | otherwise = []
handleKeyInputDuringExploring _ _ = error "We are not exploring."

handleKeyInputDuringTalking :: Game -> Text -> [AppEventResponse Game AppEvent]
handleKeyInputDuringTalking e@Game { status = Talking th } k
    | k == "Enter" = [Model $ e { status = Exploring $ finishTalking th }]
    | otherwise = []
handleKeyInputDuringTalking _ _ = error "We are not talking."

handleKeyInputDuringHandlingScene :: Game -> Text -> [AppEventResponse Game AppEvent]
handleKeyInputDuringHandlingScene e@Game { status = HandlingScene sh } k
    | k == "Enter" = [Model $ e { status = nextStatus }]
    | otherwise = []
    where nextStatus = case nextSceneOrFinish sh of
                           Right r -> HandlingScene r
                           Left l  -> Exploring l
handleKeyInputDuringHandlingScene _ _ = error "We are not handling a scene."

handleKeyInputDuringSelectingItemToUse :: Game -> Text -> [AppEventResponse Game AppEvent]
handleKeyInputDuringSelectingItemToUse e@Game { status = SelectingItemToUse sh } k
    | k == "Up" = [Model $ e { status = SelectingItemToUse $ selectPrevItem sh }]
    | k == "Down" = [Model $ e { status = SelectingItemToUse $ selectNextItem sh }]
    | k == "Enter" = [Model $ e { status = handlePlayerConsumeItem sh }]
    | k == "Esc" = [Model $ e { status = Exploring $ finishSelecting sh }]
    | otherwise = []
handleKeyInputDuringSelectingItemToUse _ _ = error "We are not selecting an item."

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
            return $ g { status = Title
                       , config = newConfig
                       }
