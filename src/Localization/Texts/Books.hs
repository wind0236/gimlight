{-# LANGUAGE OverloadedStrings #-}

module Localization.Texts.Books
    ( sampleBookContent
    ) where

import           Localization (MultilingualText, multilingualText)

sampleBookContent :: MultilingualText
sampleBookContent = multilingualText "This is a sample book." "本の実装テスト．"
