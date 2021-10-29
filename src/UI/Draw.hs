module UI.Draw
    ( drawUI
    ) where

import           Game                    (Game (Game, config, status))
import           Game.Status             (GameStatus (Exploring, GameOver, HandlingScene, SelectingItemToUse, SelectingLocale, Talking, Title))
import           UI.Draw.Exploring       (drawExploring)
import           UI.Draw.GameOver        (drawGameOver)
import           UI.Draw.Scene           (drawScene)
import           UI.Draw.SelectingItem   (drawSelectingItem)
import           UI.Draw.SelectingLocale (drawSelectingLocale)
import           UI.Draw.Talking         (drawTalking)
import           UI.Draw.Title           (drawTitle)
import           UI.Types                (GameWidgetEnv, GameWidgetNode)

drawUI :: GameWidgetEnv -> Game -> GameWidgetNode
drawUI _ Game { status = s, config = c } =
    case s of
        Exploring eh          -> drawExploring eh c
        Talking th            -> drawTalking th c
        HandlingScene hs      -> drawScene hs c
        SelectingItemToUse sh -> drawSelectingItem sh c
        Title                 -> drawTitle c
        GameOver              -> drawGameOver
        SelectingLocale       -> drawSelectingLocale
