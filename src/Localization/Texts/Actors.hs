{-# LANGUAGE OverloadedStrings #-}

module Localization.Texts.Actors
    ( player
    , electria
    , talkWithElectria
    , orc
    , troll
    ) where

import           Localization (MultilingualText, multilingualText)

player :: MultilingualText
player = multilingualText "Player" "プレイヤー"

electria :: MultilingualText
electria = multilingualText "Electria" "エレクトリア"

talkWithElectria :: MultilingualText
talkWithElectria = multilingualText "Talking test." "会話テスト．"

orc :: MultilingualText
orc = multilingualText "Orc" "オーク"

troll :: MultilingualText
troll = multilingualText "Troll" "トロール"
