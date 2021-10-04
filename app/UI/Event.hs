module UI.Event
    ( handleEvent
    ) where

import           Brick                          (Next, continue, halt)
import           Brick.Types                    (BrickEvent (VtyEvent), EventM)
import           Control.Lens.Operators         ((^?!))
import           Control.Monad.Extra            (unless)
import           Control.Monad.Trans.State.Lazy (execState, get)
import           Engine                         (Engine (HandlingScene, PlayerIsExploring, Talking, _scene),
                                                 afterFinish, afterTalking,
                                                 completeThisTurn, isGameOver,
                                                 playerBumpAction, scene)
import qualified Graphics.Vty.Input.Events      as V
import           Linear.V2                      (V2 (V2))
import           UI.Types                       (Name, Tick)

handleEvent :: Engine -> BrickEvent Name Tick -> EventM Name (Next Engine)
handleEvent e@PlayerIsExploring{} (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt e
handleEvent e@PlayerIsExploring{} (VtyEvent (V.EvKey V.KEsc []))        = halt e
handleEvent e@PlayerIsExploring{} (VtyEvent (V.EvKey V.KUp []))         = handlePlayerMove (V2 0 1) e
handleEvent e@PlayerIsExploring{} (VtyEvent (V.EvKey V.KDown []))       = handlePlayerMove (V2 0 (-1)) e
handleEvent e@PlayerIsExploring{} (VtyEvent (V.EvKey V.KRight []))      = handlePlayerMove (V2 1 0) e
handleEvent e@PlayerIsExploring{} (VtyEvent (V.EvKey V.KLeft []))       = handlePlayerMove (V2 (-1) 0) e
handleEvent e@PlayerIsExploring{} (VtyEvent (V.EvKey V.KUpRight [])) = handlePlayerMove (V2 1 1) e
handleEvent e@PlayerIsExploring{} (VtyEvent (V.EvKey V.KUpLeft [])) = handlePlayerMove (V2 1 (-1)) e
handleEvent e@PlayerIsExploring{} (VtyEvent (V.EvKey V.KDownRight [])) = handlePlayerMove (V2 (-1) 1) e
handleEvent e@PlayerIsExploring{} (VtyEvent (V.EvKey V.KDownLeft [])) = handlePlayerMove (V2 (-1) (-1)) e
handleEvent e@PlayerIsExploring{} (VtyEvent (V.EvKey (V.KChar 'k') [])) = handlePlayerMove (V2 0 1) e
handleEvent e@PlayerIsExploring{} (VtyEvent (V.EvKey (V.KChar 'j') [])) = handlePlayerMove (V2 0 (-1)) e
handleEvent e@PlayerIsExploring{} (VtyEvent (V.EvKey (V.KChar 'l') [])) = handlePlayerMove (V2 1 0) e
handleEvent e@PlayerIsExploring{} (VtyEvent (V.EvKey (V.KChar 'h') [])) = handlePlayerMove (V2 (-1) 0) e
handleEvent e@HandlingScene{} (VtyEvent (V.EvKey V.KEnter [])) = handleMessageEvent e
handleEvent e@Talking{} (VtyEvent (V.EvKey V.KEnter [])) = handleTalking e
handleEvent e _                                     = continue e

handlePlayerMove :: V2 Int -> Engine -> EventM Name (Next Engine)
handlePlayerMove d e = continue $ flip execState e $ do
    eng <- get
    let finished = eng ^?! isGameOver
    unless finished $ do
        playerBumpAction d

        eng' <- get
        case eng' of
            PlayerIsExploring {} -> completeThisTurn
            _                    -> return ()

handleMessageEvent :: Engine -> EventM Name (Next Engine)
handleMessageEvent e@HandlingScene{} =
        continue $ if length (e ^?! scene) > 1
                    then e { _scene = tail (e ^?! scene) }
                    else e ^?! afterFinish
handleMessageEvent _ = error "unreachable"

handleTalking :: Engine -> EventM Name (Next Engine)
handleTalking e@Talking{} = continue $ do
        let e' = e ^?! afterTalking
        execState completeThisTurn e'
handleTalking _ = error "unreachable"
