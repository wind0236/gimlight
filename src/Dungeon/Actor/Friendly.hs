{-# LANGUAGE OverloadedStrings #-}

module Dungeon.Actor.Friendly
    ( electria
    ) where

import           Coord         (Coord)
import           Data.Text     (Text)
import           Dungeon.Actor (Actor, ActorKind (FriendlyNpc), actor)
import           Localization  (MultilingualText, multilingualText)

electria :: Coord -> Actor
electria position = friendly position name 1 1 1 talking "images/electria.png" "images/sample_standing_picture.png"
    where name = multilingualText "Electria" "エレクトリア"
          talking = multilingualText "Talking test." "会話テスト"

friendly :: Coord -> MultilingualText -> Int -> Int -> Int -> MultilingualText -> Text -> Text -> Actor
friendly position name maxHp defence power = actor position name maxHp defence power FriendlyNpc
