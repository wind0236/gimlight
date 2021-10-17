{-# LANGUAGE OverloadedStrings #-}

module Dungeon.Entity.Friendly
    ( electria
    ) where

import           Coord          (Coord)
import           Data.Text      (Text)
import           Dungeon.Entity (ActorKind (FriendlyNpc), Entity, actor)

electria :: Coord -> Entity
electria position = friendly position "Electria" 1 1 1 "How's it going, Ruskell?" "images/electria.png" "images/sample_standing_picture.png"

friendly :: Coord -> Text -> Int -> Int -> Int -> Text -> Text -> Text -> Entity
friendly position name maxHp defence power = actor position name maxHp defence power True FriendlyNpc
