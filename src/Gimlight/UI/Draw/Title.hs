{-# LANGUAGE OverloadedStrings #-}

module Gimlight.UI.Draw.Title
    ( drawTitle
    ) where

import           Gimlight.GameConfig         (GameConfig)
import           Gimlight.Localization       (getLocalizedText)
import qualified Gimlight.Localization.Texts as T
import           Gimlight.UI.Draw.KeyEvent   (withKeyEvents)
import           Gimlight.UI.Types           (GameWidgetNode)
import           Monomer                     (CmbStyleBasic (styleBasic),
                                              CmbTextSize (textSize), label,
                                              vstack)

drawTitle :: GameConfig -> GameWidgetNode
drawTitle c =
    withKeyEvents $
    vstack
        [ label "Gimlight" `styleBasic` [textSize 36]
        , label $ "[n] " <> getLocalizedText c T.newGame
        , label $ "[l] " <> getLocalizedText c T.loadGame
        , label $ "[q] " <> getLocalizedText c T.quitGame
        ]
