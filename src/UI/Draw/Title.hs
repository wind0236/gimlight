{-# LANGUAGE OverloadedStrings #-}

module UI.Draw.Title
    ( drawTitle
    ) where

import           Game             (Game (Game, config))
import           Localization     (getLocalizedText, multilingualText)
import           Monomer          (CmbStyleBasic (styleBasic),
                                   CmbTextSize (textSize), label, vstack)
import           UI.Draw.KeyEvent (withKeyEvents)
import           UI.Types         (GameWidgetNode)

drawTitle :: Game -> GameWidgetNode
drawTitle Game { config = c } = withKeyEvents $ vstack [ label "Gimlight" `styleBasic` [textSize 36]
                                     , label $ "[n] " <> getLocalizedText c newGame
                                     , label $ "[l] " <> getLocalizedText c loadGame
                                     , label $ "[q] " <> getLocalizedText c quitGame
                                     ]
    where newGame = multilingualText "New game" "新しく始める"
          loadGame = multilingualText " Load the savedata" "セーブデータを読み込む"
          quitGame = multilingualText "Quit" "終了する"
