{-# LANGUAGE OverloadedStrings #-}

module UI.Draw.Talking
    ( drawTalking
    ) where

import           Actor              (Actor, standingImagePath)
import           Control.Lens       ((&), (.~), (^.))
import           GameConfig         (GameConfig)
import           GameStatus.Talking (TalkingHandler, finishTalking, getMessage,
                                     getTalkingPartner)
import           Localization       (MultilingualText, getLocalizedText)
import           Monomer            (CmbBgColor (bgColor),
                                     CmbPaddingL (paddingL),
                                     CmbStyleBasic (styleBasic),
                                     CmbTextColor (textColor),
                                     CmbTextSize (textSize), black, filler,
                                     gray, hstack, image, label, red, zstack)
import qualified Monomer.Lens       as L
import           UI.Draw.Exploring  (drawExploring)
import           UI.Draw.KeyEvent   (withKeyEvents)
import           UI.Types           (GameWidgetNode)

drawTalking :: TalkingHandler -> GameConfig -> GameWidgetNode
drawTalking th c =
    withKeyEvents $
    zstack
        [ drawExploring afterGameStatus c `styleBasic`
          [bgColor $ gray & L.a .~ 0.5]
        , filler `styleBasic` [bgColor $ black & L.a .~ 0.5]
        , talkingWindow c partner message
        ]
  where
    partner = getTalkingPartner th
    message = getMessage th
    afterGameStatus = finishTalking th

talkingWindow :: GameConfig -> Actor -> MultilingualText -> GameWidgetNode
talkingWindow c a msg = hstack [image (a ^. standingImagePath), window]
  where
    window =
        zstack
            [ image "images/talking_window.png"
            , label (getLocalizedText c msg) `styleBasic`
              [textColor red, textSize 16, paddingL 50]
            ]
