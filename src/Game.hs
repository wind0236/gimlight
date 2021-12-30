{-# LANGUAGE OverloadedStrings #-}

module Game
    ( start
    ) where

import           GameConfig     (getLocale, readConfigOrDefault)
import           GameModel      (GameModel (GameModel, config, status))
import           GameStatus     (GameStatus (SelectingLocale, Title))
import           Monomer        (MainWindowState (MainWindowNormal), appFontDef,
                                 appInitEvent, appTheme, appWindowResizable,
                                 appWindowState, appWindowTitle, darkTheme,
                                 startApp)
import           UI.Draw        (drawUI)
import           UI.Draw.Config (windowHeight, windowWidth)
import qualified UI.Event       as E
import           UI.Types       (AppEvent (..))

start :: IO ()
start = do
    initModel <- createModel
    startApp initModel handleEvent buildUI initUIConfig
  where
    createModel = do
        initConfig <- readConfigOrDefault
        let initStatus =
                case getLocale initConfig of
                    Just _  -> Title
                    Nothing -> SelectingLocale
        return GameModel {status = initStatus, config = initConfig}
    handleEvent = E.handleEvent
    buildUI = drawUI
    initUIConfig =
        [ appWindowTitle "Roguelike"
        , appTheme darkTheme
        , appFontDef "Regular" "third_party/noto-cjk/NotoSansCJK-VF.otf.ttc"
        , appInitEvent AppInit
        , appWindowState $ MainWindowNormal (windowWidth, windowHeight)
        , appWindowResizable False
        ]
