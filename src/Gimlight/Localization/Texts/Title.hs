{-# LANGUAGE OverloadedStrings #-}

module Gimlight.Localization.Texts.Title
  ( newGame,
    quitGame,
  )
where

import Gimlight.Localization (MultilingualText, multilingualText)

newGame :: MultilingualText
newGame = multilingualText "New game" "新しく始める"

quitGame :: MultilingualText
quitGame = multilingualText "Quit" "終了する"
