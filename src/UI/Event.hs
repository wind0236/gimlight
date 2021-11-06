{-# LANGUAGE OverloadedStrings #-}

module UI.Event
    ( handleEvent
    ) where

import           Actor.Player                   (handlePlayerConsumeItem,
                                                 handlePlayerDropItem,
                                                 handlePlayerMoving,
                                                 handlePlayerPickingUp,
                                                 handlePlayerSelectingItemToDrop,
                                                 handlePlayerSelectingItemToUse)
import           Data.Maybe                     (fromMaybe)
import           Data.Text                      (Text)
import           GameConfig                     (Language (English, Japanese),
                                                 setLocale, writeConfig)
import           GameModel                      (GameModel (GameModel, config, status))
import           GameStatus                     (GameStatus (Exploring, GameOver, ReadingBook, Scene, SelectingItemToDrop, SelectingItemToUse, SelectingLocale, Talking, Title),
                                                 newGameStatus)
import           GameStatus.Exploring           (ascendStairsAtPlayerPosition,
                                                 descendStairsAtPlayerPosition)
import           GameStatus.ReadingBook         (finishReading)
import           GameStatus.Scene               (nextSceneOrFinish)
import qualified GameStatus.SelectingItemToDrop as D
import qualified GameStatus.SelectingItemToUse  as U
import           GameStatus.Talking             (finishTalking)
import           Linear.V2                      (V2 (V2))
import           Monomer                        (EventResponse (Model, Task),
                                                 exitApplication)
import           Save                           (load, save)
import           UI.Types                       (AppEvent (AppInit, AppKeyboardInput, AppLoadFinished, AppSaveFinished),
                                                 GameEventResponse,
                                                 GameWidgetEnv, GameWidgetNode)

handleEvent ::
       GameWidgetEnv
    -> GameWidgetNode
    -> GameModel
    -> AppEvent
    -> [GameEventResponse]
handleEvent _ _ gameStatus evt =
    case evt of
        AppInit             -> []
        AppSaveFinished     -> []
        AppLoadFinished ngs -> [Model ngs]
        AppKeyboardInput k  -> handleKeyInput gameStatus k

handleKeyInput :: GameModel -> Text -> [GameEventResponse]
handleKeyInput e@GameModel {status = s} k =
    case s of
        Exploring _           -> handleKeyInputDuringExploring e k
        Talking _             -> handleKeyInputDuringTalking e k
        Scene _               -> handleKeyInputDuringScene e k
        SelectingItemToDrop _ -> handleKeyInputDuringSelectingItemToDrop e k
        SelectingItemToUse _  -> handleKeyInputDuringSelectingItemToUse e k
        ReadingBook _         -> handleKeyInputDuringReadingBook e k
        Title                 -> handleKeyInputDuringTitle e k
        SelectingLocale       -> handleKeyInputDuringSelectingLanguage e k
        GameOver              -> []

handleKeyInputDuringExploring :: GameModel -> Text -> [GameEventResponse]
handleKeyInputDuringExploring e@GameModel {status = st@(Exploring eh)} k
    | k == "Right" = [Model $ e {status = handlePlayerMoving (V2 1 0) eh}]
    | k == "Left" = [Model $ e {status = handlePlayerMoving (V2 (-1) 0) eh}]
    | k == "Up" = [Model $ e {status = handlePlayerMoving (V2 0 1) eh}]
    | k == "Down" = [Model $ e {status = handlePlayerMoving (V2 0 (-1)) eh}]
    | k == "g" = [Model e {status = handlePlayerPickingUp eh}]
    | k == "u" = [Model e {status = handlePlayerSelectingItemToUse eh}]
    | k == "d" = [Model e {status = handlePlayerSelectingItemToDrop eh}]
    | k == "Ctrl-s" = [Task (save st >> return AppSaveFinished)]
    | k == "Ctrl-l" =
        [ Task $ do
              s <- load
              return $ AppLoadFinished e {status = s}
        ]
    | k == "Shift-." =
        [ Model
              e
                  { status =
                        Exploring $
                        fromMaybe eh $ descendStairsAtPlayerPosition eh
                  }
        ]
    | k == "Shift-," =
        [ Model
              e
                  { status =
                        Exploring $
                        fromMaybe eh $ ascendStairsAtPlayerPosition eh
                  }
        ]
    | otherwise = []
handleKeyInputDuringExploring _ _ = error "We are not exploring."

handleKeyInputDuringTalking :: GameModel -> Text -> [GameEventResponse]
handleKeyInputDuringTalking e@GameModel {status = Talking th} k
    | k == "Enter" = [Model $ e {status = Exploring $ finishTalking th}]
    | otherwise = []
handleKeyInputDuringTalking _ _ = error "We are not talking."

handleKeyInputDuringScene :: GameModel -> Text -> [GameEventResponse]
handleKeyInputDuringScene e@GameModel {status = Scene sh} k
    | k == "Enter" = [Model $ e {status = nextStatus}]
    | otherwise = []
  where
    nextStatus =
        case nextSceneOrFinish sh of
            Right r -> Scene r
            Left l  -> Exploring l
handleKeyInputDuringScene _ _ = error "We are not handling a scene."

handleKeyInputDuringSelectingItemToDrop ::
       GameModel -> Text -> [GameEventResponse]
handleKeyInputDuringSelectingItemToDrop e@GameModel {status = SelectingItemToDrop sh} k
    | k == "Up" =
        [Model $ e {status = SelectingItemToDrop $ D.selectPrevItem sh}]
    | k == "Down" =
        [Model $ e {status = SelectingItemToDrop $ D.selectNextItem sh}]
    | k == "Enter" = [Model $ e {status = handlePlayerDropItem sh}]
    | k == "Esc" = [Model $ e {status = Exploring $ D.finishSelecting sh}]
    | otherwise = []
handleKeyInputDuringSelectingItemToDrop _ _ =
    error "We are not selecting an item to drop."

handleKeyInputDuringSelectingItemToUse ::
       GameModel -> Text -> [GameEventResponse]
handleKeyInputDuringSelectingItemToUse e@GameModel {status = SelectingItemToUse sh} k
    | k == "Up" =
        [Model $ e {status = SelectingItemToUse $ U.selectPrevItem sh}]
    | k == "Down" =
        [Model $ e {status = SelectingItemToUse $ U.selectNextItem sh}]
    | k == "Enter" = [Model $ e {status = handlePlayerConsumeItem sh}]
    | k == "Esc" = [Model $ e {status = Exploring $ U.finishSelecting sh}]
    | otherwise = []
handleKeyInputDuringSelectingItemToUse _ _ =
    error "We are not selecting an item."

handleKeyInputDuringReadingBook :: GameModel -> Text -> [GameEventResponse]
handleKeyInputDuringReadingBook e@GameModel {status = ReadingBook h} k
    | k == "Enter" =
        [Model $ e {status = maybe GameOver Exploring $ finishReading h}]
    | otherwise = []
handleKeyInputDuringReadingBook _ _ = error "We are not reading a book."

handleKeyInputDuringTitle :: GameModel -> Text -> [GameEventResponse]
handleKeyInputDuringTitle g k
    | k == "n" = [Task $ AppLoadFinished <$> startNewGame g]
    | k == "l" =
        [ Task $ do
              s <- load
              return $ AppLoadFinished g {status = s}
        ]
    | k == "q" = [exitApplication]
    | otherwise = []
  where
    startNewGame GameModel {config = c} = do
        st <- newGameStatus
        return GameModel {status = st, config = c}

handleKeyInputDuringSelectingLanguage ::
       GameModel -> Text -> [GameEventResponse]
handleKeyInputDuringSelectingLanguage g@GameModel {config = c} k
    | k == "e" = [Task $ AppLoadFinished <$> updateConfig English]
    | k == "j" = [Task $ AppLoadFinished <$> updateConfig Japanese]
    | otherwise = []
  where
    updateConfig l = do
        let newConfig = setLocale l c
        writeConfig newConfig
        return $ g {status = Title, config = newConfig}
