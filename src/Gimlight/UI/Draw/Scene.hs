{-# LANGUAGE OverloadedStrings #-}

module Gimlight.UI.Draw.Scene
  ( drawScene,
  )
where

import Control.Lens ((&), (.~))
import Gimlight.GameConfig (GameConfig)
import Gimlight.GameStatus.Scene
  ( SceneHandler,
    getBackgroundImagePath,
    getCurrentScene,
    text,
  )
import Gimlight.Localization (getLocalizedText)
import Gimlight.UI.Draw.Fonts (bold)
import Gimlight.UI.Draw.KeyEvent (withKeyEvents)
import Gimlight.UI.Types (GameWidgetNode)
import Monomer
  ( CmbAlignBottom (alignBottom),
    CmbAlignLeft (alignLeft),
    CmbAlignTop (alignTop),
    CmbBgColor (bgColor),
    CmbHeight (height),
    CmbMultiline (multiline),
    CmbPadding (padding),
    CmbStyleBasic (styleBasic),
    CmbTextColor (textColor),
    CmbTextFont (textFont),
    CmbTextSize (textSize),
    black,
    box_,
    filler,
    image,
    label_,
    vstack,
    white,
    zstack,
  )
import qualified Monomer.Lens as L

drawScene :: SceneHandler -> GameConfig -> GameWidgetNode
drawScene sh c =
  withKeyEvents $
    zstack
      [ image $ getBackgroundImagePath sh,
        box_ [alignBottom] (drawText sh c)
          `styleBasic` [height 10]
      ]

drawText :: SceneHandler -> GameConfig -> GameWidgetNode
drawText sh c =
  vstack
    [ filler,
      zstack
        [ filler `styleBasic` [bgColor $ black & L.a .~ 0.5],
          box_ [alignTop, alignLeft] $
            label_ (getLocalizedText c $ text $ getCurrentScene sh) [multiline]
              `styleBasic` [textColor white, textSize 20, padding 20, textFont bold]
        ]
        `styleBasic` [height 200]
    ]
