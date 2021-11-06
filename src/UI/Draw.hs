module UI.Draw
    ( drawUI
    ) where

import           GameModel                   (GameModel (GameModel, config, status))
import           GameStatus                  (GameStatus (Exploring, GameOver, ReadingBook, Scene, SelectingItemToDrop, SelectingItemToUse, SelectingLocale, Talking, Title))
import           UI.Draw.Exploring           (drawExploring)
import           UI.Draw.GameOver            (drawGameOver)
import           UI.Draw.ReadingBook         (drawReadingBook)
import           UI.Draw.Scene               (drawScene)
import           UI.Draw.SelectingItemToDrop (drawSelectingItemToDrop)
import           UI.Draw.SelectingItemToUse  (drawSelectingItemToUse)
import           UI.Draw.SelectingLocale     (drawSelectingLocale)
import           UI.Draw.Talking             (drawTalking)
import           UI.Draw.Title               (drawTitle)
import           UI.Types                    (GameWidgetEnv, GameWidgetNode)

drawUI :: GameWidgetEnv -> GameModel -> GameWidgetNode
drawUI _ GameModel {status = s, config = c} =
    case s of
        Exploring eh           -> drawExploring eh c
        Talking th             -> drawTalking th c
        Scene hs               -> drawScene hs c
        SelectingItemToUse sh  -> drawSelectingItemToUse sh c
        SelectingItemToDrop sh -> drawSelectingItemToDrop sh c
        ReadingBook h          -> drawReadingBook h c
        Title                  -> drawTitle c
        GameOver               -> drawGameOver
        SelectingLocale        -> drawSelectingLocale
