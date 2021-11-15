{-# LANGUAGE OverloadedStrings #-}

module UI.Draw.Talking
    ( drawTalking
    ) where

import           Actor                   (Actor, standingImagePath)
import           Control.Lens            ((&), (.~), (^.))
import           GameConfig              (GameConfig)
import           GameStatus.Talking      (TalkingHandler, getExploringHandler,
                                          getTalkingPart, getTalkingPartner)
import           GameStatus.Talking.Part (SelectionHandler,
                                          TalkingPart (Selection), getChoices,
                                          getQuestion, getSelectingIndex)
import           Localization            (getLocalizedText)
import           Monomer                 (CmbAlignCenter (alignCenter),
                                          CmbBgColor (bgColor),
                                          CmbStyleBasic (styleBasic),
                                          CmbTextColor (textColor),
                                          CmbTextSize (textSize), black, box_,
                                          filler, gray, hstack, image, label,
                                          red, vstack, zstack)
import qualified Monomer.Lens            as L
import           UI.Draw.Exploring       (drawExploring)
import           UI.Draw.KeyEvent        (withKeyEvents)
import           UI.Types                (GameWidgetNode)

drawTalking :: TalkingHandler -> GameConfig -> GameWidgetNode
drawTalking th c =
    withKeyEvents $
    zstack
        [ drawExploring afterGameStatus c `styleBasic`
          [bgColor $ gray & L.a .~ 0.5]
        , filler `styleBasic` [bgColor $ black & L.a .~ 0.5]
        , talkingWindow c partner (getTalkingPart th)
        ]
  where
    partner = getTalkingPartner th
    afterGameStatus = getExploringHandler th

talkingWindow :: GameConfig -> Actor -> TalkingPart -> GameWidgetNode
talkingWindow c a (Selection h) =
    box_ [alignCenter] $ hstack [image (a ^. standingImagePath), window]
  where
    window = zstack [image "images/talking_window.png", talkingContent c h]
talkingWindow _ _ _ = error "Unable to draw."

talkingContent :: GameConfig -> SelectionHandler -> GameWidgetNode
talkingContent c h =
    box_ [alignCenter] $
    vstack
        ((label (getLocalizedText c $ getQuestion h) `styleBasic`
          [textColor red, textSize 16]) :
         map
             ((`styleBasic` [textColor red, textSize 16]) . label)
             (listWithAsteriskMark $ map (getLocalizedText c) $ getChoices h))
  where
    listWithAsteriskMark =
        zipWith
            (\idx x ->
                 if getSelectingIndex h == Just idx
                     then "* " <> x
                     else "  " <> x)
            [0 ..]
