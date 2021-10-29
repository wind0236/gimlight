{-# LANGUAGE OverloadedStrings #-}

module UI.Draw.GameOver
    ( drawGameOver
    ) where

import           Monomer  (CmbStyleBasic (styleBasic), CmbTextSize (textSize),
                           label, vstack)
import           UI.Types (GameWidgetNode)

drawGameOver :: GameWidgetNode
drawGameOver = vstack [label "Game Over" `styleBasic` [textSize 72]]
