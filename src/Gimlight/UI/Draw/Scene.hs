module Gimlight.UI.Draw.Scene
    ( drawScene
    ) where

import           Gimlight.GameConfig       (GameConfig)
import           Gimlight.GameStatus.Scene (SceneHandler,
                                            getBackgroundImagePath,
                                            getCurrentScene, text)
import           Gimlight.Localization     (getLocalizedText)
import           Gimlight.UI.Draw.KeyEvent (withKeyEvents)
import           Gimlight.UI.Types         (GameWidgetNode)
import           Monomer                   (CmbMultiline (multiline),
                                            CmbStyleBasic (styleBasic),
                                            CmbTextColor (textColor), black,
                                            image, label_, zstack)

drawScene :: SceneHandler -> GameConfig -> GameWidgetNode
drawScene sh c =
    withKeyEvents $
    zstack
        [ image $ getBackgroundImagePath sh
        , label_ (getLocalizedText c $ text $ getCurrentScene sh) [multiline] `styleBasic`
          [textColor black]
        ]
