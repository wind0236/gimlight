module UI.Draw.Scene
    ( drawScene
    ) where

import           GameConfig       (GameConfig)
import           GameStatus.Scene (SceneHandler, getBackgroundImagePath,
                                   getCurrentScene, text)
import           Localization     (getLocalizedText)
import           Monomer          (CmbMultiline (multiline),
                                   CmbStyleBasic (styleBasic),
                                   CmbTextColor (textColor), black, image,
                                   label_, zstack)
import           UI.Draw.KeyEvent (withKeyEvents)
import           UI.Types         (GameWidgetNode)

drawScene :: SceneHandler -> GameConfig -> GameWidgetNode
drawScene sh c =
    withKeyEvents $
    zstack
        [ image $ getBackgroundImagePath sh
        , label_ (getLocalizedText c $ text $ getCurrentScene sh) [multiline] `styleBasic`
          [textColor black]
        ]
