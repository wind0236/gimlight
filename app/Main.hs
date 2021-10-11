{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Engine   (initEngine)
import           Monomer  (MainWindowState (MainWindowNormal), appFontDef,
                           appInitEvent, appTheme, appWindowResizable,
                           appWindowState, appWindowTitle, darkTheme, startApp)
import           UI.Draw  (drawUI, windowHeight, windowWidth)
import qualified UI.Event as E
import           UI.Types (AppEvent (..))

main :: IO ()
main = startApp model handleEvent buildUI config
    where
        model = initEngine
        handleEvent = E.handleEvent
        buildUI = drawUI
        config = [ appWindowTitle "Roguelike"
                 , appTheme darkTheme
                 , appFontDef "Regular" "third_party/noto-cjk/NotoSansCJK-VF.otf.ttc"
                 , appInitEvent AppInit
                 , appWindowState $ MainWindowNormal (windowWidth, windowHeight)
                 , appWindowResizable False
                 ]
