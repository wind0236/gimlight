{-# LANGUAGE OverloadedStrings #-}

module UI.Event
    ( handleEvent
    ) where

import           Data.Maybe                          (fromMaybe)
import           Data.Text                           (Text)
import           Dungeon.Actor.Player                (handlePlayerConsumeItem,
                                                      handlePlayerMoving,
                                                      handlePlayerPickingUp,
                                                      handlePlayerSelectingItemToUse)
import           GameModel                           (GameModel (GameModel, config, status))
import           GameModel.Config                    (Language (English, Japanese),
                                                      setLocale, writeConfig)
import           GameModel.Status                    (GameStatus (Exploring, GameOver, HandlingScene, SelectingItemToUse, SelectingLocale, Talking, Title),
                                                      newGameStatus)
import           GameModel.Status.Exploring          (ascendStairsAtPlayerPosition,
                                                      descendStairsAtPlayerPosition)
import           GameModel.Status.Scene              (nextSceneOrFinish)
import           GameModel.Status.SelectingItemToUse (finishSelecting,
                                                      selectNextItem,
                                                      selectPrevItem)
import           GameModel.Status.Talking            (finishTalking)
import           Linear.V2                           (V2 (V2))
import           Monomer                             (AppEventResponse,
                                                      EventResponse (Model, Task),
                                                      WidgetEnv, WidgetNode,
                                                      exitApplication)
import           Save                                (load, save)
import           UI.Types                            (AppEvent (AppInit, AppKeyboardInput, AppLoadFinished, AppSaveFinished))

handleEvent :: WidgetEnv GameModel AppEvent -> WidgetNode GameModel AppEvent -> GameModel -> AppEvent -> [AppEventResponse GameModel AppEvent]
handleEvent _ _ gameStatus evt =
    case evt of
        AppInit             -> []
        AppSaveFinished     -> []
        AppLoadFinished ngs -> [Model ngs]
        AppKeyboardInput k  -> handleKeyInput gameStatus k

handleKeyInput :: GameModel -> Text -> [AppEventResponse GameModel AppEvent]
handleKeyInput e@GameModel { status = s } k =
    case s of
        Exploring _          -> handleKeyInputDuringExploring e k
        Talking _            -> handleKeyInputDuringTalking e k
        HandlingScene _      -> handleKeyInputDuringHandlingScene e k
        SelectingItemToUse _ -> handleKeyInputDuringSelectingItemToUse e k
        Title                -> handleKeyInputDuringTitle e k
        SelectingLocale      -> handleKeyInputDuringSelectingLanguage e k
        GameOver             -> []


handleKeyInputDuringExploring :: GameModel -> Text -> [AppEventResponse GameModel AppEvent]
handleKeyInputDuringExploring e@GameModel { status = st@(Exploring eh) } k
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
    | k == "Shift-." = [Model e { status = Exploring $ fromMaybe eh $ descendStairsAtPlayerPosition eh }]
    | k == "Shift-," = [Model e { status = Exploring $ fromMaybe eh $ ascendStairsAtPlayerPosition eh }]
    | otherwise = []
handleKeyInputDuringExploring _ _ = error "We are not exploring."

handleKeyInputDuringTalking :: GameModel -> Text -> [AppEventResponse GameModel AppEvent]
handleKeyInputDuringTalking e@GameModel { status = Talking th } k
    | k == "Enter" = [Model $ e { status = Exploring $ finishTalking th }]
    | otherwise = []
handleKeyInputDuringTalking _ _ = error "We are not talking."

handleKeyInputDuringHandlingScene :: GameModel -> Text -> [AppEventResponse GameModel AppEvent]
handleKeyInputDuringHandlingScene e@GameModel { status = HandlingScene sh } k
    | k == "Enter" = [Model $ e { status = nextStatus }]
    | otherwise = []
    where nextStatus = case nextSceneOrFinish sh of
                           Right r -> HandlingScene r
                           Left l  -> Exploring l
handleKeyInputDuringHandlingScene _ _ = error "We are not handling a scene."

handleKeyInputDuringSelectingItemToUse :: GameModel -> Text -> [AppEventResponse GameModel AppEvent]
handleKeyInputDuringSelectingItemToUse e@GameModel { status = SelectingItemToUse sh } k
    | k == "Up" = [Model $ e { status = SelectingItemToUse $ selectPrevItem sh }]
    | k == "Down" = [Model $ e { status = SelectingItemToUse $ selectNextItem sh }]
    | k == "Enter" = [Model $ e { status = handlePlayerConsumeItem sh }]
    | k == "Esc" = [Model $ e { status = Exploring $ finishSelecting sh }]
    | otherwise = []
handleKeyInputDuringSelectingItemToUse _ _ = error "We are not selecting an item."

handleKeyInputDuringTitle :: GameModel -> Text -> [AppEventResponse GameModel AppEvent]
handleKeyInputDuringTitle g k
    | k == "n" = [Task $ AppLoadFinished <$> startNewGame g]
    | k == "l" = [Task $ do
                    s <- load
                    return $ AppLoadFinished g { status = s }]
    | k == "q" = [exitApplication]
    | otherwise = []
    where startNewGame GameModel { config = c } =
            do
                st <- newGameStatus
                return GameModel { status = st
                                 , config = c
                                 }

handleKeyInputDuringSelectingLanguage :: GameModel -> Text -> [AppEventResponse GameModel AppEvent]
handleKeyInputDuringSelectingLanguage g@GameModel { config = c } k
    | k == "e" = [Task $ AppLoadFinished <$> updateConfig English]
    | k == "j" = [Task $ AppLoadFinished <$> updateConfig Japanese]
    | otherwise = []
    where updateConfig l = do
            let newConfig = setLocale l c
            writeConfig newConfig
            return $ g { status = Title
                       , config = newConfig
                       }
