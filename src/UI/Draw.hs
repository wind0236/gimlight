module UI.Draw
    ( drawUI
    ) where

import           GameModel               (GameModel (GameModel, config, graphics, status))
import           GameStatus              (GameStatus (Exploring, GameOver, ReadingBook, Scene, SelectingItem, SelectingLocale, Talking, Title))
import           UI.Draw.Exploring       (drawExploring)
import           UI.Draw.GameOver        (drawGameOver)
import           UI.Draw.ReadingBook     (drawReadingBook)
import           UI.Draw.Scene           (drawScene)
import           UI.Draw.SelectingItem   (drawSelectingItem)
import           UI.Draw.SelectingLocale (drawSelectingLocale)
import           UI.Draw.Talking         (drawTalking)
import           UI.Draw.Title           (drawTitle)
import           UI.Graphics             (getMapTiles)
import           UI.Types                (GameWidgetEnv, GameWidgetNode)

drawUI :: GameWidgetEnv -> GameModel -> GameWidgetNode
drawUI _ GameModel {status = s, config = c, graphics = g} =
    case s of
        Exploring eh    -> drawExploring (getMapTiles g) eh c
        Talking th      -> drawTalking (getMapTiles g) th c
        Scene hs        -> drawScene hs c
        SelectingItem h -> drawSelectingItem h c
        ReadingBook h   -> drawReadingBook h c
        Title           -> drawTitle c
        GameOver        -> drawGameOver
        SelectingLocale -> drawSelectingLocale
