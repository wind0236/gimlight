{-# LANGUAGE OverloadedStrings #-}

module Gimlight.UI.Draw.SelectingLocale
  ( drawSelectingLocale,
  )
where

import Gimlight.UI.Draw.KeyEvent (withKeyEvents)
import Gimlight.UI.Types (GameWidgetNode)
import Monomer (label, vstack)

drawSelectingLocale :: GameWidgetNode
drawSelectingLocale =
  withKeyEvents $
    vstack
      [ label "Choose your language. / 言語を選択してください．",
        label "[e] English",
        label "[j] 日本語"
      ]
