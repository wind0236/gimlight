module UI(main) where

import           Brick                      (App (..), AttrMap, AttrName,
                                             BrickEvent (AppEvent, VtyEvent),
                                             EventM, Next, Padding (Pad),
                                             Widget, attrMap, bg, continue,
                                             customMain, fg, hBox, hLimit, halt,
                                             neverShowCursor, on, padAll,
                                             padTop, str, strWrap, vBox, vLimit,
                                             withAttr, withBorderStyle, (<+>),
                                             (<=>))
import           Brick.BChan                (newBChan, writeBChan)
import qualified Brick.Widgets.Border       as B
import qualified Brick.Widgets.Border.Style as BS
import           Brick.Widgets.Center       (center)
import qualified Brick.Widgets.Center       as C
import           Control.Concurrent         (forkIO, threadDelay)
import           Control.Lens               (use, (&), (^.), (^?!))
import           Control.Monad              (forever, unless, void, when)
import           Control.Monad.Trans.State  (execState, get, put)
import           Data.Array.Base            ((!))
import           Data.List                  (sortOn)
import           Data.Maybe                 (fromMaybe)
import           Dungeon.Size               (height, width)
import           Dungeon.Types              (char, entities, entityAttr,
                                             explored, name, position,
                                             renderOrder, tileMap, visible)
import           Engine                     (Engine (HandlingScene, PlayerIsExploring, Talking, _scene),
                                             afterFinish, afterTalking,
                                             completeThisTurn, dungeon,
                                             initEngine, isGameOver, messageLog,
                                             playerBumpAction, playerCurrentHp,
                                             playerMaxHp, scene, talk)
import qualified Graphics.Vty               as V
import           Linear.V2                  (V2 (..), _x, _y)
import qualified Log                        as L
import           Map.Tile                   (darkAttr, lightAttr)
import qualified Map.Tile                   as T
import           Scene                      (SceneElement (WithSpeaker, WithoutSpeaker))
import           Talking                    (destruct, talkWith)
import           UI.Attrs                   (attrMapForThisGame, emptyAttr,
                                             greenAttr, redAttr)

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
          , appAttrMap = const attrMapForThisGame
          }

handleEvent :: Engine -> BrickEvent Name Tick -> EventM Name (Next Engine)
handleEvent e@PlayerIsExploring{} (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt e
handleEvent e@PlayerIsExploring{} (VtyEvent (V.EvKey V.KEsc []))        = halt e
handleEvent e@PlayerIsExploring{} (VtyEvent (V.EvKey V.KUp []))         = handlePlayerMove (V2 0 1) e
handleEvent e@PlayerIsExploring{} (VtyEvent (V.EvKey V.KDown []))       = handlePlayerMove (V2 0 (-1)) e
handleEvent e@PlayerIsExploring{} (VtyEvent (V.EvKey V.KRight []))      = handlePlayerMove (V2 1 0) e
handleEvent e@PlayerIsExploring{} (VtyEvent (V.EvKey V.KLeft []))       = handlePlayerMove (V2 (-1) 0) e
handleEvent e@PlayerIsExploring{} (VtyEvent (V.EvKey V.KUpRight [])) = handlePlayerMove (V2 1 1) e
handleEvent e@PlayerIsExploring{} (VtyEvent (V.EvKey V.KUpLeft [])) = handlePlayerMove (V2 1 (-1)) e
handleEvent e@PlayerIsExploring{} (VtyEvent (V.EvKey V.KDownRight [])) = handlePlayerMove (V2 (-1) 1) e
handleEvent e@PlayerIsExploring{} (VtyEvent (V.EvKey V.KDownLeft [])) = handlePlayerMove (V2 (-1) (-1)) e
handleEvent e@PlayerIsExploring{} (VtyEvent (V.EvKey (V.KChar 'k') [])) = handlePlayerMove (V2 0 1) e
handleEvent e@PlayerIsExploring{} (VtyEvent (V.EvKey (V.KChar 'j') [])) = handlePlayerMove (V2 0 (-1)) e
handleEvent e@PlayerIsExploring{} (VtyEvent (V.EvKey (V.KChar 'l') [])) = handlePlayerMove (V2 1 0) e
handleEvent e@PlayerIsExploring{} (VtyEvent (V.EvKey (V.KChar 'h') [])) = handlePlayerMove (V2 (-1) 0) e
handleEvent e@HandlingScene{} (VtyEvent (V.EvKey V.KEnter [])) = handleMessageEvent e
handleEvent e@Talking{} (VtyEvent (V.EvKey V.KEnter [])) = handleTalking e
handleEvent e _                                     = continue e

handlePlayerMove :: V2 Int -> Engine -> EventM Name (Next Engine)
handlePlayerMove d e = continue $ flip execState e $ do
    eng <- get
    let finished = eng ^?! isGameOver
    unless finished $ do
        playerBumpAction d

        eng <- get
        case eng of
            PlayerIsExploring {} -> completeThisTurn
            _                    -> return ()

drawUI :: Engine -> [Widget Name]
drawUI e@PlayerIsExploring{} = [ C.center $ drawHpBar e <+> (padTop (Pad 2) (drawGame e) <=> drawMessageLog e)]
drawUI engine@Talking{} = [withBorderStyle BS.unicodeBold
    $ B.borderWithLabel (str "Roguelike game")
    $ center
    $ padAll 2
    $ strWrap m]
    where
        (e, s) = destruct $ engine ^?! talk
        m = (e ^. name) ++ ": " ++ s
drawUI engine@HandlingScene{} = [withBorderStyle BS.unicodeBold
    $ B.borderWithLabel (str "Roguelike game")
    $ center
    $ padAll 2
    $ strWrap m]
    where
        m = case head $ engine ^?! scene of
                WithSpeaker name msg -> name ++ ": " ++ msg
                WithoutSpeaker msg   -> msg

drawGame :: Engine -> Widget Name
drawGame engine@PlayerIsExploring{} = withBorderStyle BS.unicodeBold
    $ B.borderWithLabel (str "Game")
    $ vBox rows
    where
        d = engine ^?! dungeon
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
        cellAt c = let entityAt = sortOn (^. renderOrder) $ entityOnCellAt c
                       in case entityAt of
                        entity:_ | visibleAt c -> withAttr (entity ^. entityAttr) $ str $ entity ^. char
                        _        -> withAttr (attrAt c) $ str [tileOnCellAt c ^. T.char]
drawGame _ = error "unreachable."

drawMessageLog :: Engine -> Widget Name
drawMessageLog engine = withBorderStyle BS.unicodeBold
    $ B.borderWithLabel (str "Log")
    $ vBox rows
    where
        rows = reverse $ fmap (\(attr, s) -> withAttr attr $ str s) $ take L.height $ concatMap (reverse . L.messageToAttrNameAndStringList) (engine ^. messageLog)

drawStatus :: Engine -> [Widget Name]
drawStatus e = [ C.center $ padTop (Pad 2) (drawHpBar e)]

drawHpBar :: Engine -> Widget Name
drawHpBar e = let barWidth = 20
                  currentHp = playerCurrentHp e
                  maxHp = playerMaxHp e
                  filledWidth = currentHp * barWidth `div` maxHp
                  attrAt x = if x < filledWidth then greenAttr else redAttr
              in vBox [hBox $ map (\x -> withAttr (attrAt x) $ str "XX") [0 .. barWidth - 1], str $ "HP: " ++ show currentHp ++ " / " ++ show maxHp]

handleMessageEvent :: Engine -> EventM Name (Next Engine)
handleMessageEvent e@HandlingScene{} =
        continue $ if length (e ^?! scene) > 1
                    then e { _scene = tail (e ^?! scene) }
                    else e ^?! afterFinish
handleMessageEvent _ = error "unreachable"

handleTalking :: Engine -> EventM Name (Next Engine)
handleTalking e@Talking{} = continue $ do
        let e' = e ^?! afterTalking
        execState completeThisTurn e'
handleTalking _ = error "unreachable"
