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
import           UI.Draw                    (drawUI)
import           UI.Event                   (handleEvent)
import           UI.Types                   (Name, Tick (Tick))

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
