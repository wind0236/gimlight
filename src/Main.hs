{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Game     (afterBooting)
import           Monomer  (MainWindowState (MainWindowNormal), appFontDef,
                           appInitEvent, appTheme, appWindowResizable,
                           appWindowState, appWindowTitle, darkTheme, startApp)
import           UI.Draw  (drawUI, windowHeight, windowWidth)
import qualified UI.Event as E
import           UI.Types (AppEvent (..))

main :: IO ()
main = do
    initModel <- afterBooting

    startApp initModel handleEvent buildUI config
    where
        handleEvent = E.handleEvent
        buildUI = drawUI
        config = [ appWindowTitle "Roguelike"
                 , appTheme darkTheme
                 , appFontDef "Regular" "third_party/noto-cjk/NotoSansCJK-VF.otf.ttc"
                 , appInitEvent AppInit
                 , appWindowState $ MainWindowNormal (windowWidth, windowHeight)
                 , appWindowResizable False
                 ]
