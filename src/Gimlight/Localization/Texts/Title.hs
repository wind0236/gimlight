{-# LANGUAGE OverloadedStrings #-}

module Gimlight.Localization.Texts.Title
  ( newGame,
    loadGame,
    quitGame,
  )
where

import Gimlight.Localization (MultilingualText, multilingualText)

newGame :: MultilingualText
newGame = multilingualText "New game" "新しく始める"

loadGame :: MultilingualText
loadGame = multilingualText "Load the savedata" "セーブデータを読み込む"

quitGame :: MultilingualText
quitGame = multilingualText "Quit" "終了する"
