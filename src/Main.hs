{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main
    ( main
    ) where

import           GameConfig     (getLocale, readConfigOrDefault)
import           GameModel      (GameModel (GameModel, config, graphics, status))
import           GameStatus     (GameStatus (SelectingLocale, Title))
import           Monomer        (MainWindowState (MainWindowNormal), appFontDef,
                                 appInitEvent, appTheme, appWindowResizable,
                                 appWindowState, appWindowTitle, darkTheme,
                                 startApp)
import           UI.Draw        (drawUI)
import           UI.Draw.Config (windowHeight, windowWidth)
import qualified UI.Event       as E
import qualified UI.Graphics    as Graphics
import           UI.Types       (AppEvent (..))

main :: IO ()
main = do
    initModel <- createModel
    startApp initModel handleEvent buildUI initUIConfig
  where
    createModel = do
        initConfig <- readConfigOrDefault
        gr <-
            (\case
                 Just x  -> x
                 Nothing -> error "Failed to load the image files.") <$>
            Graphics.graphics
        let initStatus =
                case getLocale initConfig of
                    Just _  -> Title
                    Nothing -> SelectingLocale
        return
            GameModel {status = initStatus, config = initConfig, graphics = gr}
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
