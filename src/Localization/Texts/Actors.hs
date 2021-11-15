{-# LANGUAGE OverloadedStrings #-}

module Localization.Texts.Actors
    ( player
    , electria
    , yes
    , no
    , orc
    , troll
    , module Localization.Texts.Actors.Electria
    ) where

import           Localization                       (MultilingualText,
                                                     multilingualText)
import           Localization.Texts.Actors.Electria

player :: MultilingualText
player = multilingualText "Player" "プレイヤー"

electria :: MultilingualText
electria = multilingualText "Electria" "エレクトリア"

yes :: MultilingualText
yes = multilingualText "Yes" "はい"

no :: MultilingualText
no = multilingualText "No" "いいえ"

orc :: MultilingualText
orc = multilingualText "Orc" "オーク"

troll :: MultilingualText
troll = multilingualText "Troll" "トロール"
