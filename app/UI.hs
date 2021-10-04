module UI(main) where

import           Brick               (neverShowCursor)
import           Brick.BChan         (newBChan, writeBChan)
import           Brick.Main          (App (App, appAttrMap, appChooseCursor, appDraw, appHandleEvent, appStartEvent),
                                      customMain)
import           Control.Concurrent  (forkIO, threadDelay)
import           Control.Monad.Extra (forever, void)
import           Engine              (Engine, initEngine)
import qualified Graphics.Vty        as V
import           UI.Attrs            (attrMapForThisGame)
import           UI.Draw             (drawUI)
import           UI.Event            (handleEvent)
import           UI.Types            (Name, Tick (Tick))

main :: IO ()
main = do
        chan <- newBChan 10
        _ <- forkIO $ forever $ do
                writeBChan chan Tick
                threadDelay 100000
        g <- initEngine
        let builder = V.mkVty V.defaultConfig
        initialVty <- builder
        void $ customMain initialVty builder (Just chan) app g

app :: App Engine Tick Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const attrMapForThisGame
          }
