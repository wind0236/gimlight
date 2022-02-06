module Gimlight.UI.Draw
    ( drawUI
    ) where

import           Gimlight.GameModel               (GameModel (GameModel, config, status))
import           Gimlight.GameStatus              (GameStatus (Exploring, GameOver, ReadingBook, Scene, SelectingItem, SelectingLocale, Talking, Title))
import           Gimlight.UI.Draw.Exploring       (drawExploring)
import           Gimlight.UI.Draw.GameOver        (drawGameOver)
import           Gimlight.UI.Draw.ReadingBook     (drawReadingBook)
import           Gimlight.UI.Draw.Scene           (drawScene)
import           Gimlight.UI.Draw.SelectingItem   (drawSelectingItem)
import           Gimlight.UI.Draw.SelectingLocale (drawSelectingLocale)
import           Gimlight.UI.Draw.Talking         (drawTalking)
import           Gimlight.UI.Draw.Title           (drawTitle)
import           Gimlight.UI.Types                (GameWidgetEnv,
                                                   GameWidgetNode)

drawUI :: GameWidgetEnv -> GameModel -> GameWidgetNode
drawUI _ GameModel {status = s, config = c} =
    case s of
        Exploring eh    -> drawExploring eh c
        Talking th      -> drawTalking th c
        Scene hs        -> drawScene hs c
        SelectingItem h -> drawSelectingItem h c
        ReadingBook h   -> drawReadingBook h c
        Title           -> drawTitle c
        GameOver        -> drawGameOver
        SelectingLocale -> drawSelectingLocale
