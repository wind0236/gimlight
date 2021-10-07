module UI.Draw
    ( drawUI
    ) where
import           Data.Bifunctor (second)
import           Data.Text      (pack)
import           Engine         (Engine)
import           Monomer        (CmbStyleBasic (styleBasic), CmbWidth (width),
                                 WidgetEnv, WidgetNode, keyDown, keyLeft,
                                 keyRight, keyUp, keystroke, vstack)
import           UI.Draw.Map    (mapGrid)
import           UI.Types       (AppEvent (AppKeyboardInput))

drawUI :: WidgetEnv Engine AppEvent -> Engine -> WidgetNode Engine AppEvent
drawUI _ engine = withKeyEvents $ vstack [ mapGrid engine
                                         ] `styleBasic` [width 0]
    where withKeyEvents =
            keystroke $ map (second AppKeyboardInput)
            [ (pack "Up", keyUp)
            , (pack "Down", keyDown)
            , (pack "Right", keyRight)
            , (pack "Left", keyLeft)
            ]
