{-# LANGUAGE OverloadedStrings #-}

module Gimlight.UI.Draw.GameOver
    ( drawGameOver
    ) where

import           Gimlight.UI.Types (GameWidgetNode)
import           Monomer           (CmbStyleBasic (styleBasic),
                                    CmbTextSize (textSize), label, vstack)

drawGameOver :: GameWidgetNode
drawGameOver = vstack [label "Game Over" `styleBasic` [textSize 72]]
