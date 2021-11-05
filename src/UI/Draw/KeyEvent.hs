{-# LANGUAGE OverloadedStrings #-}

module UI.Draw.KeyEvent
    ( withKeyEvents
    ) where

import           Monomer  (WidgetNode, keystroke)
import           UI.Types (AppEvent (AppKeyboardInput))

withKeyEvents :: WidgetNode s AppEvent -> WidgetNode s AppEvent
withKeyEvents =
    keystroke $
    map (\x -> (x, AppKeyboardInput x))
        [ "Up"
        , "Down"
        , "Right"
        , "Left"
        , "Enter"
        , "Shift-."
        , "Shift-,"
        , "d"
        , "n"
        , "l"
        , "q"
        , "g"
        , "u"
        , "e"
        , "j"
        , "Ctrl-s"
        , "Ctrl-l"
        , "Esc"
        ]
