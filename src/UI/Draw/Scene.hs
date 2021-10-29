module UI.Draw.Scene
    ( drawScene
    ) where

import           Control.Lens      ((^.))
import           Game              (Game (Game, config, status))
import           Game.Status       (GameStatus (HandlingScene))
import           Game.Status.Scene (destructHandler)
import           Localization      (getLocalizedText)
import           Monomer           (CmbMultiline (multiline),
                                    CmbStyleBasic (styleBasic),
                                    CmbTextColor (textColor), black, image,
                                    label_, zstack)
import           Scene             (backgroundImage, elements, text)
import           UI.Draw.KeyEvent  (withKeyEvents)
import           UI.Types          (GameWidgetNode)

drawScene :: Game -> GameWidgetNode
drawScene Game { status = HandlingScene sh, config = c } =
    withKeyEvents $ zstack [ image (s ^. backgroundImage)
                           , label_  (getLocalizedText c $ text $ head $ s ^. elements) [multiline] `styleBasic` [textColor black]
                           ]
    where (s, _) = destructHandler sh
drawScene _ = error "We are not handling a scene."
