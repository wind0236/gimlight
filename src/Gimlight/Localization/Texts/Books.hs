{-# LANGUAGE OverloadedStrings #-}

module Gimlight.Localization.Texts.Books
    ( sampleBookContent
    ) where

import           Gimlight.Localization (MultilingualText, multilingualText)

sampleBookContent :: MultilingualText
sampleBookContent = multilingualText "This is a sample book." "本の実装テスト．"
