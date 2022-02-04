{-# LANGUAGE OverloadedStrings #-}

module Gimlight.Localization.Texts.Scene
  ( title1,
    title2,
    welcome,
  )
where

import Gimlight.Localization (MultilingualText, multilingualText)

title1 :: MultilingualText
title1 = multilingualText "This is the English text 1." "これは日本語テキスト1です．"

title2 :: MultilingualText
title2 = multilingualText "And this is the English text 2." "そしてこれは日本語テキスト2です．"

welcome :: MultilingualText
welcome =
  multilingualText "Welcome to the world of Gimlight!" "Gimlightの世界へようこそ！"
