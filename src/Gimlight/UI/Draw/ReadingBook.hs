module Gimlight.UI.Draw.ReadingBook
  ( drawReadingBook,
  )
where

import Gimlight.GameConfig (GameConfig)
import Gimlight.GameStatus.ReadingBook
  ( ReadingBookHandler,
    getContent,
  )
import Gimlight.Localization (getLocalizedText)
import Gimlight.UI.Draw.KeyEvent (withKeyEvents)
import Gimlight.UI.Types (GameWidgetNode)
import Monomer
  ( label_,
    multiline,
    styleBasic,
    textColor,
    white,
    zstack,
  )

drawReadingBook :: ReadingBookHandler -> GameConfig -> GameWidgetNode
drawReadingBook h c =
  withKeyEvents $
    zstack
      [ label_ (getLocalizedText c $ getContent h) [multiline]
          `styleBasic` [textColor white]
      ]
