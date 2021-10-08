module UI.Event
    ( handleEvent
    ) where

import           Control.Lens              ((^?!))
import           Control.Monad             (unless)
import           Control.Monad.Trans.State (execState, get)
import           Engine                    (Engine (PlayerIsExploring),
                                            completeThisTurn, isGameOver,
                                            playerBumpAction)
import           Linear.V2                 (V2 (V2))
import           Monomer                   (AppEventResponse,
                                            EventResponse (Model), KeyCode,
                                            WidgetEnv, WidgetNode, keyDown,
                                            keyLeft, keyRight, keyUp)
import           UI.Types                  (AppEvent (AppInit, AppKeyboardInput))

handleEvent :: WidgetEnv Engine AppEvent -> WidgetNode Engine AppEvent -> Engine -> AppEvent -> [AppEventResponse Engine AppEvent]
handleEvent _ _ engine evt = case evt of
                                AppInit            -> []
                                AppKeyboardInput k -> [Model $ handleKeyInput engine k]

handleKeyInput :: Engine -> KeyCode -> Engine
handleKeyInput e@PlayerIsExploring{} k
    | k == keyRight = handlePlayerMove (V2 1 0) e
    | k == keyLeft  = handlePlayerMove (V2 (-1) 0) e
    | k == keyUp    = handlePlayerMove (V2 0 1) e
    | k == keyDown  = handlePlayerMove (V2 0 (-1)) e
    | otherwise = e
handleKeyInput e _ = e

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
