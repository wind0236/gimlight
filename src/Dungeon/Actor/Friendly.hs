{-# LANGUAGE OverloadedStrings #-}

module Dungeon.Actor.Friendly
    ( electria
    ) where

import           Coord                   (Coord)
import           Data.Text               (Text)
import           Dungeon.Actor           (Actor, ActorKind (FriendlyNpc), actor)
import           Dungeon.Actor.Status    (Status, status)
import           Dungeon.Actor.Status.Hp (hp)
import           Localization            (MultilingualText, multilingualText)

electria :: Coord -> Actor
electria position = friendly position name st talking "images/electria.png" "images/sample_standing_picture.png"
    where st = status (hp 1) 1 1
          name = multilingualText "Electria" "エレクトリア"
          talking = multilingualText "Talking test." "会話テスト"

friendly :: Coord -> MultilingualText -> Status -> MultilingualText -> Text -> Text -> Actor
friendly position name st = actor position name st FriendlyNpc
