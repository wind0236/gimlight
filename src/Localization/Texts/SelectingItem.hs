{-# LANGUAGE OverloadedStrings #-}

module Localization.Texts.SelectingItem
    ( whichItemToUse
    ) where

import           Localization (MultilingualText, multilingualText)

whichItemToUse :: MultilingualText
whichItemToUse = multilingualText "Which item do you use?" "どのアイテムを使う？"
