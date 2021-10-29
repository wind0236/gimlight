{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Game           (Game (Game, config, status))
import           Game.Config    (getLocale, readConfigOrDefault)
import           Game.Status    (GameStatus (SelectingLocale, Title))
import           Monomer        (MainWindowState (MainWindowNormal), appFontDef,
                                 appInitEvent, appTheme, appWindowResizable,
                                 appWindowState, appWindowTitle, darkTheme,
                                 startApp)
import           UI.Draw        (drawUI)
import           UI.Draw.Config (windowHeight, windowWidth)
import qualified UI.Event       as E
import           UI.Types       (AppEvent (..))

main :: IO ()
main = do
    initModel <- createModel

    startApp initModel handleEvent buildUI initUIConfig
    where
        createModel = do
            initConfig <- readConfigOrDefault
            let initStatus = case getLocale initConfig of
                                 Just _  -> Title
                                 Nothing -> SelectingLocale
            return Game { status = initStatus
                        , config = initConfig
                        }
        handleEvent = E.handleEvent
        buildUI = drawUI
        initUIConfig = [ appWindowTitle "Roguelike"
                       , appTheme darkTheme
                       , appFontDef "Regular" "third_party/noto-cjk/NotoSansCJK-VF.otf.ttc"
                       , appInitEvent AppInit
                       , appWindowState $ MainWindowNormal (windowWidth, windowHeight)
                       , appWindowResizable False
                       ]
