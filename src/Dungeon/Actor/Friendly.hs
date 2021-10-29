{-# LANGUAGE OverloadedStrings #-}

module Dungeon.Actor.Friendly
    ( electria
    ) where

import           Coord                   (Coord)
import           Data.Text               (Text)
import           Dungeon.Actor           (Actor, ActorKind (FriendlyNpc), actor)
import           Dungeon.Actor.Status    (Status, status)
import           Dungeon.Actor.Status.Hp (hp)
import           Localization            (MultilingualText)
import qualified Localization.Texts      as T

electria :: Coord -> Actor
electria position = friendly position T.electria st T.talkWithElectria "images/electria.png" "images/sample_standing_picture.png"
    where st = status (hp 1) 1 1

friendly :: Coord -> MultilingualText -> Status -> MultilingualText -> Text -> Text -> Actor
friendly position name st = actor position name st FriendlyNpc
