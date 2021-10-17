{-# LANGUAGE OverloadedStrings #-}

module Dungeon.Actor.Friendly
    ( electria
    ) where

import           Coord         (Coord)
import           Data.Text     (Text)
import           Dungeon.Actor (Actor, ActorKind (FriendlyNpc), actor)

electria :: Coord -> Actor
electria position = friendly position "Electria" 1 1 1 "How's it going, Ruskell?" "images/electria.png" "images/sample_standing_picture.png"

friendly :: Coord -> Text -> Int -> Int -> Int -> Text -> Text -> Text -> Actor
friendly position name maxHp defence power = actor position name maxHp defence power FriendlyNpc
