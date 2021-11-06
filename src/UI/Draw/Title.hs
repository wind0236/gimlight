{-# LANGUAGE OverloadedStrings #-}

module UI.Draw.Title
    ( drawTitle
    ) where

import           GameConfig         (GameConfig)
import           Localization       (getLocalizedText)
import qualified Localization.Texts as T
import           Monomer            (CmbStyleBasic (styleBasic),
                                     CmbTextSize (textSize), label, vstack)
import           UI.Draw.KeyEvent   (withKeyEvents)
import           UI.Types           (GameWidgetNode)

drawTitle :: GameConfig -> GameWidgetNode
drawTitle c =
    withKeyEvents $
    vstack
        [ label "Gimlight" `styleBasic` [textSize 36]
        , label $ "[n] " <> getLocalizedText c T.newGame
        , label $ "[l] " <> getLocalizedText c T.loadGame
        , label $ "[q] " <> getLocalizedText c T.quitGame
        ]
