{-# LANGUAGE OverloadedStrings #-}

module UI.Event
    ( handleEvent
    ) where

import           Control.Monad.Trans.State (execState)
import           Data.Text                 (Text)
import           GameStatus                (GameStatus,
                                            enterTownAtPlayerPosition,
                                            finishTalking, isHandlingScene,
                                            isPlayerExploring, isPlayerTalking,
                                            isTitle, newGameStatus,
                                            nextSceneElementOrFinish)
import qualified GameStatus                as GS
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
    | isTitle e = handleKeyInputDuringTitle k
    | otherwise = undefined

handleKeyInputDuringExploring :: GameStatus -> Text -> [AppEventResponse GameStatus AppEvent]
handleKeyInputDuringExploring e k
    | k == "Right" = [Model $ handlePlayerMoving (V2 1 0) e]
    | k == "Left"  = [Model $ handlePlayerMoving (V2 (-1) 0) e]
    | k == "Up"    = [Model $ handlePlayerMoving (V2 0 1) e]
    | k == "Down"  = [Model $ handlePlayerMoving (V2 0 (-1)) e]
    | k == "g" = [Model $ handlePlayerPickingUp e]
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

handleKeyInputDuringTitle :: Text -> [AppEventResponse GameStatus AppEvent]
handleKeyInputDuringTitle k
    | k == "n" = [Task $ AppLoadFinished <$> newGameStatus]
    | k == "l" = [Task $ AppLoadFinished <$> load]
    | k == "q" = [exitApplication]
    | otherwise = []

handlePlayerMoving :: V2 Int -> GameStatus -> GameStatus
handlePlayerMoving offset e = flip execState e $ GS.handlePlayerMoving offset

handlePlayerPickingUp :: GameStatus -> GameStatus
handlePlayerPickingUp = execState GS.handlePlayerPickingUp
