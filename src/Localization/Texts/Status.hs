{-# LANGUAGE OverloadedStrings #-}

module Localization.Texts.Status
    ( attack
    , defence
    ) where

import           Localization (MultilingualText, multilingualText)

attack :: MultilingualText
attack = multilingualText "Attack" "攻撃"

defence :: MultilingualText
defence = multilingualText  "Defence" "防御"
