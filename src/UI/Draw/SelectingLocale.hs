{-# LANGUAGE OverloadedStrings #-}

module UI.Draw.SelectingLocale
    ( drawSelectingLocale
    ) where

import           Monomer          (label, vstack)
import           UI.Draw.KeyEvent (withKeyEvents)
import           UI.Types         (GameWidgetNode)

drawSelectingLocale :: GameWidgetNode
drawSelectingLocale =
    withKeyEvents $
    vstack
        [ label "Choose your language. / 言語を選択してください．"
        , label "[e] English"
        , label "[j] 日本語"
        ]
