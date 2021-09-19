{-# LANGUAGE OverloadedStrings #-}

module UI(main) where

import           Brick                      (App (..), AttrMap, AttrName,
                                             BrickEvent (VtyEvent), EventM,
                                             Next, Widget, attrMap, continue,
                                             customMain, hBox, halt,
                                             neverShowCursor, on, str, vBox,
                                             withBorderStyle)
import           Brick.BChan                (newBChan, writeBChan)
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center       as C
import           Control.Concurrent         (forkIO, threadDelay)
import           Control.Lens               ((^.))
import           Control.Monad              (forever, void)
import           Game                       (Game, height, initGame, player,
                                             width)
import qualified Graphics.Vty               as V
import           Linear.V2                  (V2 (..))

main :: IO ()
main = do
        chan <- newBChan 10
        forkIO $ forever $ do
            writeBChan chan Tick
            threadDelay 100000
        g <- initGame
        let builder = V.mkVty V.defaultConfig
        initialVty <- builder
        void $ customMain initialVty builder (Just chan) app g

data Tick = Tick

app :: App Game Tick Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleEvent g (VtyEvent (V.EvKey V.KEsc []))        = halt g
handleEvent g _                                     = continue g

type Name = ()

drawUI :: Game -> [Widget Name]
drawUI g = [ C.center $ drawGame g ]

drawGame :: Game -> Widget Name
drawGame g = withBorderStyle BS.unicodeBold $ vBox rows
    where
        rows = [hBox $ cellsInRow r | r <- [height - 1, height -2 .. 0]]
        cellsInRow y = [putCoord (V2 x y)  | x <- [ 0 .. width - 1]]
        putCoord = str . cellAt
        cellAt c = if g ^. player == c
                    then "@"
                    else ""

theMap :: AttrMap
theMap = attrMap V.defAttr
    [(playerAttr, V.blue `on` V.black)]

playerAttr, emptyAttr :: AttrName
playerAttr = "playerAttr"
emptyAttr = "emptyAttr"
