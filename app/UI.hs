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
import           Direction                  (Direction (East, North, South, West))
import           Dungeon                    (entities, explored, gameMap,
                                             visible)
import           Dungeon.Size               (height, width)
import           Dungeon.Tile               (darkAttr, lightAttr)
import           Entity                     (char, entityAttr, position)
import           Game                       (Game, dungeon, initGame,
                                             movePlayer, updateMap)
import qualified Graphics.Vty               as V
import           Linear.V2                  (V2 (..), _x, _y)

data Tick = Tick

type Name = ()

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
handleEvent g (VtyEvent (V.EvKey V.KUp []))         = continue $ updateMap $ movePlayer North g
handleEvent g (VtyEvent (V.EvKey V.KDown []))       = continue $ updateMap $ movePlayer South g
handleEvent g (VtyEvent (V.EvKey V.KRight []))      = continue $ updateMap $ movePlayer East g
handleEvent g (VtyEvent (V.EvKey V.KLeft []))       = continue $ updateMap $ movePlayer West g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'k') [])) = continue $ updateMap $ movePlayer North g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'j') [])) = continue $ updateMap $ movePlayer South g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'l') [])) = continue $ updateMap $ movePlayer East g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'h') [])) = continue $ updateMap $ movePlayer West g
handleEvent g _                                     = continue g

drawUI :: Game -> [Widget Name]
drawUI g = [ C.center $ drawGame g ]

drawGame :: Game -> Widget Name
drawGame g = withBorderStyle BS.unicodeBold
    $ B.borderWithLabel (str "Game")
    $ vBox rows
    where
        d = g ^. dungeon
        rows = [hBox $ cellsInRow r | r <- [height - 1, height - 2 .. 0]]
        cellsInRow y = [cellAt (V2 x y)  | x <- [0 .. width - 1]]
        coordAsTuple c = (c ^. _x, c ^. _y)
        entityOnCellAt c = [e | e <- entities d, e ^. position == c]
        visibleAt c = (d ^. visible) ! coordAsTuple c
        exploredAt c = (d ^. explored) ! coordAsTuple c
        tileOnCellAt c = (d ^. gameMap) ! coordAsTuple c
        attrAt c
          | visibleAt c = tileOnCellAt c ^. lightAttr
          | exploredAt c = tileOnCellAt c ^. darkAttr
          | otherwise = emptyAttr
        cellAt c = let entityAt = entityOnCellAt c
                       in case entityAt
                       of
                        entity:_ | visibleAt c -> withAttr (entity ^. entityAttr) $ str $ entity ^. char
                        _        -> withAttr (attrAt c) $ str " "

theMap :: AttrMap
theMap = attrMap V.defAttr
    [ (playerAttr, fg V.brightWhite)
    , (npcAttr, fg V.yellow)
    , (orcAttr, fg $ V.rgbColor 63 127 63)
    , (trollAttr, fg $ V.rgbColor 0 127 0)
    , (darkFloorAttr, V.rgbColor 255 255 255 `on` V.rgbColor 50 50 150)
    , (lightFloorAttr, V.rgbColor 255 255 255 `on` V.rgbColor 200 180 50)
    , (darkWallAttr, V.rgbColor 255 255 255 `on` V.rgbColor 0 0 100)
    , (lightWallAttr, V.rgbColor 255 255 255 `on` V.rgbColor 130 110 50)
    ]

playerAttr, npcAttr, emptyAttr, darkFloorAttr, darkWallAttr, orcAttr, trollAttr :: AttrName
playerAttr = "playerAttr"
npcAttr = "npcAttr"
emptyAttr = "emptyAttr"
darkFloorAttr = "darkFloorAttr"
lightFloorAttr = "lightFloorAttr"
darkWallAttr = "darkWallAttr"
lightWallAttr = "lightWallAttr"
orcAttr = "orcAttr"
trollAttr = "trollAttr"
