{-# LANGUAGE OverloadedStrings #-}

module Gimlight.Localization.Texts.Items
    ( herb
    , sampleBook
    ) where

import           Gimlight.Localization (MultilingualText, multilingualText)

herb :: MultilingualText
herb = multilingualText "Herb" "薬草"

sampleBook :: MultilingualText
sampleBook = multilingualText "Sample book" "テスト用本"
