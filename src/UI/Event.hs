{-# LANGUAGE OverloadedStrings #-}

module UI.Event
    ( handleEvent
    ) where

import           Control.Lens              ((%=), (%~), (&), (&~), (.=), (.~),
                                            (^.), (^?!))
import           Control.Monad             (unless)
import           Control.Monad.Trans.State (execState, get, runState)
import           Data.Text                 (Text)
import           Dungeon                   (initialPlayerPositionCandidates,
                                            popPlayer, updateMap)
import           Dungeon.Types             (entities, position)
import           Engine                    (Engine (HandlingScene, PlayerIsExploring, Talking, Title),
                                            completeThisTurn, currentDungeon,
                                            isGameOver, newGameEngine,
                                            otherDungeons, playerBumpAction,
                                            popDungeonAtPlayerPosition)
import           Linear.V2                 (V2 (V2))
import           Monomer                   (AppEventResponse,
                                            EventResponse (Model, Task),
                                            WidgetEnv, WidgetNode,
                                            exitApplication)
import           Save                      (load, save)
import           Scene                     (elements)
import           UI.Types                  (AppEvent (AppInit, AppKeyboardInput, AppLoadFinished, AppSaveFinished))

handleEvent :: WidgetEnv Engine AppEvent -> WidgetNode Engine AppEvent -> Engine -> AppEvent -> [AppEventResponse Engine AppEvent]
handleEvent _ _ engine evt = case evt of
                                AppInit            -> []
                                AppSaveFinished    -> []
                                AppLoadFinished newEngine  -> [Model newEngine]
                                AppKeyboardInput k -> handleKeyInput engine k

handleKeyInput :: Engine -> Text -> [AppEventResponse Engine AppEvent]
handleKeyInput e@PlayerIsExploring{} k
    | k == "Right" = [Model $ handlePlayerMove (V2 1 0) e]
    | k == "Left"  = [Model $ handlePlayerMove (V2 (-1) 0) e]
    | k == "Up"    = [Model $ handlePlayerMove (V2 0 1) e]
    | k == "Down"  = [Model $ handlePlayerMove (V2 0 (-1)) e]
    | k == "Ctrl-s"     = [Task (save e >> return AppSaveFinished)]
    | k == "Ctrl-l"     = [Task $ AppLoadFinished <$> load]
    | k == "Enter" = [Model $ handleEnter e]
    | otherwise = []
handleKeyInput (Talking _ after) k
    | k == "Enter" = [Model after]
    | otherwise = []
handleKeyInput e@HandlingScene{} k
    | k == "Enter" = [Model $ nextSceneElementOrFinish e]
handleKeyInput Title k
    | k == "n" = [Task $ AppLoadFinished <$> newGameEngine]
    | k == "l" = [Task $ AppLoadFinished <$> load]
    | k == "q" = [exitApplication]
handleKeyInput _ _ = []

handlePlayerMove :: V2 Int -> Engine -> Engine
handlePlayerMove offset e = flip execState e $ do
    eng <- get
    let finished = eng ^?! isGameOver
    unless finished $ do
        playerBumpAction offset

        eng' <- get
        case eng' of
            PlayerIsExploring {} -> completeThisTurn
            _                    -> return ()

handleEnter :: Engine -> Engine
handleEnter e = case popDungeonAtPlayerPosition e of
                    (Just d, e') -> e' &~ do
                        let newPosition = head $ initialPlayerPositionCandidates d
                        let (p, currentDungeon') = runState popPlayer (e ^?! currentDungeon)
                        otherDungeons %= (:) currentDungeon'
                        currentDungeon .= (d & entities %~ (:) (p & position .~ newPosition))
                        currentDungeon %= execState updateMap
                    (Nothing, _) -> e

nextSceneElementOrFinish :: Engine -> Engine
nextSceneElementOrFinish (HandlingScene s after) = if length (s ^. elements) == 1
                                                    then after
                                                    else HandlingScene (s & elements %~ tail) after
nextSceneElementOrFinish _                   = error "unreachable."
