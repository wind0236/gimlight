module UI.Draw.ReadingBook
    ( drawReadingBook
    ) where

import           GameConfig             (GameConfig)
import           GameStatus.ReadingBook (ReadingBookHandler, getContent)
import           Localization           (getLocalizedText)
import           Monomer                (label_, multiline, styleBasic,
                                         textColor, white, zstack)
import           UI.Draw.KeyEvent       (withKeyEvents)
import           UI.Types               (GameWidgetNode)

drawReadingBook :: ReadingBookHandler -> GameConfig -> GameWidgetNode
drawReadingBook h c =
    withKeyEvents $
    zstack
        [ label_ (getLocalizedText c $ getContent h) [multiline] `styleBasic`
          [textColor white]
        ]
