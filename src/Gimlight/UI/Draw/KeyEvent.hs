{-# LANGUAGE OverloadedStrings #-}

module Gimlight.UI.Draw.KeyEvent
  ( withKeyEvents,
  )
where

import Gimlight.UI.Types (AppEvent (AppKeyboardInput))
import Monomer (WidgetNode, keystroke)

withKeyEvents :: WidgetNode s AppEvent -> WidgetNode s AppEvent
withKeyEvents =
  keystroke $
    map
      (\x -> (x, AppKeyboardInput x))
      [ "Up",
        "Down",
        "Right",
        "Left",
        "Enter",
        "Shift-.",
        "Shift-,",
        "d",
        "n",
        "l",
        "q",
        "g",
        "u",
        "e",
        "j",
        "Ctrl-s",
        "Ctrl-l",
        "Esc"
      ]
