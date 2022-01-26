{-# LANGUAGE OverloadedStrings #-}

module Gimlight.Localization.Texts.Actors
    ( player
    , electria
    , yes
    , no
    , orc
    , troll
    , module Gimlight.Localization.Texts.Actors.Electria
    ) where

import           Gimlight.Localization                       (MultilingualText,
                                                              multilingualText)
import           Gimlight.Localization.Texts.Actors.Electria

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
