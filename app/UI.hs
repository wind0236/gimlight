{-# LANGUAGE OverloadedStrings #-}

module UI(main) where

import           Brick                      (App (..), AttrMap, AttrName,
                                             BrickEvent (AppEvent, VtyEvent),
                                             EventM, Next, Padding (Pad),
                                             Widget, attrMap, continue,
                                             customMain, fg, hBox, hLimit, halt,
                                             neverShowCursor, on, padAll,
                                             padTop, str, vBox, vLimit,
                                             withAttr, withBorderStyle, (<=>))
import           Brick.BChan                (newBChan, writeBChan)
import qualified Brick.Widgets.Border       as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center       as C
import           Control.Concurrent         (forkIO, threadDelay)
import           Control.Lens               ((&), (^.))
import           Control.Monad              (forever, void)
import           Data.Array.Base            ((!))
import           Direction                  (Direction (East, North, South, West))
import           Dungeon                    (entities, explored, tileMap,
                                             visible)
import           Dungeon.Map.Tile           (darkAttr, lightAttr)
import           Dungeon.Size               (height, width)
import           Engine                     (Engine, completeThisTurn, dungeon,
                                             initEngine, messageLog,
                                             playerBumpAction)
import           Entity                     (char, entityAttr, position)
import qualified Graphics.Vty               as V
import           Linear.V2                  (V2 (..), _x, _y)
import qualified Log                        as L

data Tick = Tick

type Name = ()

main :: IO ()
main = do
        chan <- newBChan 10
        forkIO $ forever $ do
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
          , appAttrMap = const theMap
          }

handleEvent :: Engine -> BrickEvent Name Tick -> EventM Name (Next Engine)
handleEvent e (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt e
handleEvent e (VtyEvent (V.EvKey V.KEsc []))        = halt e
handleEvent e (VtyEvent (V.EvKey V.KUp []))         = handlePlayerMove North e
handleEvent e (VtyEvent (V.EvKey V.KDown []))       = handlePlayerMove South e
handleEvent e (VtyEvent (V.EvKey V.KRight []))      = handlePlayerMove East e
handleEvent e (VtyEvent (V.EvKey V.KLeft []))       = handlePlayerMove West e
handleEvent e (VtyEvent (V.EvKey (V.KChar 'k') [])) = handlePlayerMove North e
handleEvent e (VtyEvent (V.EvKey (V.KChar 'j') [])) = handlePlayerMove South e
handleEvent e (VtyEvent (V.EvKey (V.KChar 'l') [])) = handlePlayerMove East e
handleEvent e (VtyEvent (V.EvKey (V.KChar 'h') [])) = handlePlayerMove West e
handleEvent e _                                     = continue e

handlePlayerMove :: Direction -> Engine -> EventM Name (Next Engine)
handlePlayerMove d e = continue $ completeThisTurn $ playerBumpAction d e

drawUI :: Engine -> [Widget Name]
drawUI g = [ C.center $ padTop (Pad 2) (drawGame g) <=> drawMessageLog g ]

drawGame :: Engine -> Widget Name
drawGame g = withBorderStyle BS.unicodeBold
    $ B.borderWithLabel (str "Game")
    $ vBox rows
    where
        d = g ^. dungeon
        rows = [hBox $ cellsInRow r | r <- [height - 1, height - 2 .. 0]]
        cellsInRow y = [cellAt (V2 x y)  | x <- [0 .. width - 1]]
        coordAsTuple c = (c ^. _x, c ^. _y)
        entityOnCellAt c = [e | e <- d ^. entities, e ^. position == c]
        visibleAt c = (d ^. visible) ! coordAsTuple c
        exploredAt c = (d ^. explored) ! coordAsTuple c
        tileOnCellAt c = (d ^. tileMap) ! coordAsTuple c
        attrAt c
          | visibleAt c = tileOnCellAt c ^. lightAttr
          | exploredAt c = tileOnCellAt c ^. darkAttr
          | otherwise = emptyAttr
        cellAt c = let entityAt = entityOnCellAt c
                       in case entityAt
                       of
                        entity:_ | visibleAt c -> withAttr (entity ^. entityAttr) $ str $ entity ^. char
                        _        -> withAttr (attrAt c) $ str " "

drawMessageLog :: Engine -> Widget Name
drawMessageLog g = withBorderStyle BS.unicodeBold
    $ B.borderWithLabel (str "Log")
    $ vBox rows
    where
        rows = [m | m <- reverse $ fmap (\(attr, s) -> withAttr attr $ str s) $ take L.height $ concatMap (reverse . L.messageToAttrNameAndStringList) (g ^. messageLog)]

theMap :: AttrMap
theMap = attrMap V.defAttr
    [ (playerAttr, V.black `on` V.rgbColor 200 180 50)
    , (orcAttr, V.rgbColor 63 127 63 `on` V.rgbColor 200 180 50)
    , (trollAttr, V.rgbColor 0 127 0 `on` V.rgbColor 200 180 50)
    , (darkFloorAttr, V.rgbColor 255 255 255 `on` V.rgbColor 50 50 150)
    , (lightFloorAttr, V.rgbColor 255 255 255 `on` V.rgbColor 200 180 50)
    , (darkWallAttr, V.rgbColor 255 255 255 `on` V.rgbColor 0 0 100)
    , (lightWallAttr, V.rgbColor 255 255 255 `on` V.rgbColor 130 110 50)
    , (attackMessageAttr, fg V.red)
    , (infoMessageAttr, fg V.blue)
    ]

playerAttr, npcAttr, emptyAttr, darkFloorAttr, darkWallAttr, orcAttr, trollAttr, infoMessageAttr :: AttrName
playerAttr = "playerAttr"
npcAttr = "npcAttr"
emptyAttr = "emptyAttr"
darkFloorAttr = "darkFloorAttr"
lightFloorAttr = "lightFloorAttr"
darkWallAttr = "darkWallAttr"
lightWallAttr = "lightWallAttr"
orcAttr = "orcAttr"
trollAttr = "trollAttr"
attackMessageAttr = "attackMessageAttr"
infoMessageAttr = "infoMessageAttr"
