{-# LANGUAGE OverloadedStrings #-}

module UI(main) where

import           Brick                      (App (..), AttrMap, AttrName,
                                             BrickEvent (AppEvent, VtyEvent),
                                             EventM, Next, Widget, attrMap,
                                             continue, customMain, fg, hBox,
                                             halt, neverShowCursor, on, str,
                                             vBox, withAttr, withBorderStyle)
import           Brick.BChan                (newBChan, writeBChan)
import qualified Brick.Widgets.Border       as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center       as C
import           Control.Concurrent         (forkIO, threadDelay)
import           Control.Lens               ((&), (^.))
import           Control.Monad              (forever, void)
import           Data.Array.Base            ((!))
import           Game                       (Direction (..), Game, char,
                                             entities, entityAttr, gameMap,
                                             height, initGame, move, player,
                                             position, tileAttr, width)
import qualified Graphics.Vty               as V
import           Linear.V2                  (V2 (..), _x, _y)

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
handleEvent g (VtyEvent (V.EvKey V.KUp []))         = continue $ move North g
handleEvent g (VtyEvent (V.EvKey V.KDown []))       = continue $ move South g
handleEvent g (VtyEvent (V.EvKey V.KRight []))      = continue $ move East g
handleEvent g (VtyEvent (V.EvKey V.KLeft []))       = continue $ move West g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'k') [])) = continue $ move North g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'j') [])) = continue $ move South g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'l') [])) = continue $ move East g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'h') [])) = continue $ move West g
handleEvent g _                                     = continue g

type Name = ()

drawUI :: Game -> [Widget Name]
drawUI g = [ C.center $ drawGame g ]

drawGame :: Game -> Widget Name
drawGame g = withBorderStyle BS.unicodeBold
    $ B.borderWithLabel (str "Game")
    $ vBox rows
    where
        rows = [hBox $ cellsInRow r | r <- [height - 1, height - 2 .. 0]]
        cellsInRow y = [cellAt (V2 x y)  | x <- [0 .. width - 1]]
        tileOnCellAt c = (g ^. gameMap) ! (c ^. _x, c ^. _y)
        entityOnCellAt c = [e | e <- entities g, e ^. position == c]
        cellAt c = let entityAt = entityOnCellAt c
                       in case entityAt
                       of
                        entity:_ -> withAttr (entity ^. entityAttr) $ str $ entity ^. char
                        []       -> withAttr (tileOnCellAt c ^. tileAttr) $ str " "

theMap :: AttrMap
theMap = attrMap V.defAttr
    [ (playerAttr, fg V.brightWhite)
    , (npcAttr, fg V.yellow)
    , (floorAttr, V.rgbColor 255 255 255 `on` V.rgbColor 50 50 150)
    , (wallAttr, V.rgbColor 255 255 255 `on` V.rgbColor 0 0 100)
    ]

playerAttr, npcAttr, emptyAttr, floorAttr, wallAttr :: AttrName
playerAttr = "playerAttr"
npcAttr = "npcAttr"
emptyAttr = "emptyAttr"
floorAttr = "floorAttr"
wallAttr = "wallAttr"
