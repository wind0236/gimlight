{-# LANGUAGE OverloadedStrings #-}

module Dungeon.Actor.Friendly
    ( electria
    ) where

import           Coord         (Coord)
import           Data.Text     (Text)
import           Dungeon.Actor (Actor, ActorKind (FriendlyNpc), actor)
import           Localization  (MultilingualText, multilingualText)

electria :: Coord -> Actor
electria position = case f of
                        Just x  -> x
                        Nothing -> error "Failed to create the Electria actor."

    where f = friendly position name 1 1 1 talking "images/electria.png" "images/sample_standing_picture.png"
          name = multilingualText "Electria" "エレクトリア"
          talking = multilingualText "Talking test." "会話テスト"

friendly :: Coord -> MultilingualText -> Int -> Int -> Int -> MultilingualText -> Text -> Text -> Maybe Actor
friendly position name maxHp defence power = actor position name maxHp defence power FriendlyNpc
