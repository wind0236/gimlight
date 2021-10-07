module Main (main) where

import           Data.Text (pack)
import           Engine    (initEngine)
import           Monomer   (appInitEvent, appTheme, appWindowTitle, darkTheme,
                            startApp)
import           UI.Draw   (drawUI)
import qualified UI.Event  as E
import           UI.Types  (AppEvent (..))

main :: IO ()
main = startApp model handleEvent buildUI config
    where
        model = initEngine
        handleEvent = E.handleEvent
        buildUI = drawUI
        config = [ appWindowTitle (pack "Roguelike")
                 , appTheme darkTheme
                 , appInitEvent AppInit
                 ]
