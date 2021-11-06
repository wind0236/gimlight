{-# LANGUAGE OverloadedStrings #-}

module UI.Draw.Talking
    ( drawTalking
    ) where

import           Actor              (standingImagePath)
import           Control.Lens       ((&), (.~), (^.))
import           GameModel.Config   (Config)
import           GameStatus.Talking (TalkingHandler, destructHandler)
import           Localization       (getLocalizedText)
import           Monomer            (CmbBgColor (bgColor),
                                     CmbPaddingL (paddingL),
                                     CmbStyleBasic (styleBasic),
                                     CmbTextColor (textColor),
                                     CmbTextSize (textSize), black, filler,
                                     gray, hstack, image, label, red, zstack)
import qualified Monomer.Lens       as L
import           Talking            (TalkWith, message, person)
import           UI.Draw.Exploring  (drawExploring)
import           UI.Draw.KeyEvent   (withKeyEvents)
import           UI.Types           (GameWidgetNode)

drawTalking :: TalkingHandler -> Config -> GameWidgetNode
drawTalking th c =
    withKeyEvents $
    zstack
        [ drawExploring afterGameStatus c `styleBasic`
          [bgColor $ gray & L.a .~ 0.5]
        , filler `styleBasic` [bgColor $ black & L.a .~ 0.5]
        , talkingWindow c with
        ]
  where
    (with, afterGameStatus) = destructHandler th

talkingWindow :: Config -> TalkWith -> GameWidgetNode
talkingWindow c tw = hstack [image (tw ^. person . standingImagePath), window]
  where
    window =
        zstack
            [ image "images/talking_window.png"
            , label (getLocalizedText c (tw ^. message)) `styleBasic`
              [textColor red, textSize 16, paddingL 50]
            ]
