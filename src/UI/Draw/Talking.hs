{-# LANGUAGE OverloadedStrings #-}

module UI.Draw.Talking
    ( drawTalking
    ) where

import           Control.Lens        ((&), (.~), (^.))
import           Dungeon.Actor       (standingImagePath)
import           Game                (Game (Game, config, status))
import           Game.Status         (GameStatus (Exploring, Talking))
import           Game.Status.Talking (destructHandler)
import           Localization        (getLocalizedText)
import           Monomer             (CmbBgColor (bgColor),
                                      CmbPaddingL (paddingL),
                                      CmbStyleBasic (styleBasic),
                                      CmbTextColor (textColor),
                                      CmbTextSize (textSize), black, filler,
                                      gray, hstack, image, label, red, zstack)
import qualified Monomer.Lens        as L
import           Talking             (TalkWith, message, person)
import           UI.Draw.Exploring   (drawExploring)
import           UI.Draw.KeyEvent    (withKeyEvents)
import           UI.Types            (GameWidgetNode)

drawTalking ::  Game -> GameWidgetNode
drawTalking e@Game { status = Talking th } =
    withKeyEvents $ zstack [ drawExploring (e { status = Exploring afterGameStatus }) `styleBasic` [bgColor $ gray & L.a .~ 0.5]
                           , filler `styleBasic` [bgColor $ black & L.a .~ 0.5]
                           , talkingWindow e with
                           ]
    where (with, afterGameStatus) = destructHandler th
drawTalking _ = error "We are not handling a talk event."

talkingWindow :: Game -> TalkWith -> GameWidgetNode
talkingWindow Game { config = c } tw = hstack [ image (tw ^. person . standingImagePath)
                            , window
                            ]
    where window = zstack [ image "images/talking_window.png"
                          , label (getLocalizedText c (tw ^. message)) `styleBasic` [textColor red, textSize 16, paddingL 50]
                          ]
