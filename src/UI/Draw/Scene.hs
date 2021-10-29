module UI.Draw.Scene
    ( drawScene
    ) where

import           Control.Lens      ((^.))
import           Game.Config       (Config)
import           Game.Status.Scene (SceneHandler, destructHandler)
import           Localization      (getLocalizedText)
import           Monomer           (CmbMultiline (multiline),
                                    CmbStyleBasic (styleBasic),
                                    CmbTextColor (textColor), black, image,
                                    label_, zstack)
import           Scene             (backgroundImage, elements, text)
import           UI.Draw.KeyEvent  (withKeyEvents)
import           UI.Types          (GameWidgetNode)

drawScene :: SceneHandler -> Config -> GameWidgetNode
drawScene sh c =
    withKeyEvents $ zstack [ image (s ^. backgroundImage)
                           , label_  (getLocalizedText c $ text $ head $ s ^. elements) [multiline] `styleBasic` [textColor black]
                           ]
    where (s, _) = destructHandler sh
