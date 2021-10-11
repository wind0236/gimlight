module UI.Event
    ( handleEvent
    ) where

import           Control.Lens              ((%~), (&), (^.), (^?!))
import           Control.Monad             (unless)
import           Control.Monad.Trans.State (execState, get)
import           Engine                    (Engine (HandlingScene, PlayerIsExploring, Talking),
                                            completeThisTurn, isGameOver,
                                            playerBumpAction)
import           Linear.V2                 (V2 (V2))
import           Monomer                   (AppEventResponse,
                                            EventResponse (Model, Task),
                                            KeyCode, WidgetEnv, WidgetNode,
                                            keyDown, keyL, keyLeft, keyReturn,
                                            keyRight, keyS, keyUp)
import           Save                      (load, save)
import           Scene                     (elements)
import           UI.Types                  (AppEvent (AppInit, AppKeyboardInput, AppLoadFinished, AppSaveFinished))

handleEvent :: WidgetEnv Engine AppEvent -> WidgetNode Engine AppEvent -> Engine -> AppEvent -> [AppEventResponse Engine AppEvent]
handleEvent _ _ engine evt = case evt of
                                AppInit            -> []
                                AppSaveFinished    -> []
                                AppLoadFinished newEngine  -> [Model newEngine]
                                AppKeyboardInput k -> if k == keyS
                                                          then [Task (do
                                                                        save engine
                                                                        return AppInit)]
                                                          else handleKeyInput engine k

handleKeyInput :: Engine -> KeyCode -> [AppEventResponse Engine AppEvent]
handleKeyInput e@PlayerIsExploring{} k
    | k == keyRight = [Model $ handlePlayerMove (V2 1 0) e]
    | k == keyLeft  = [Model $ handlePlayerMove (V2 (-1) 0) e]
    | k == keyUp    = [Model $ handlePlayerMove (V2 0 1) e]
    | k == keyDown  = [Model $ handlePlayerMove (V2 0 (-1)) e]
    | k == keyS     = [Task (save e >> return AppSaveFinished)]
    | k == keyL     = [Task $ AppLoadFinished <$> load]
    | otherwise = []
handleKeyInput (Talking _ after) k
    | k == keyReturn = [Model after]
    | otherwise = []
handleKeyInput e@HandlingScene{} k
    | k == keyReturn = [Model $ nextSceneElementOrFinish e]
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

nextSceneElementOrFinish :: Engine -> Engine
nextSceneElementOrFinish (HandlingScene s after) = if length (s ^. elements) == 1
                                                    then after
                                                    else HandlingScene (s & elements %~ tail) after
nextSceneElementOrFinish _                   = error "unreachable."
