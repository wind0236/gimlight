{-# LANGUAGE OverloadedStrings #-}

module Dungeon.Actor.Monsters
    ( orc
    , troll
    ) where
import           Coord                   (Coord)
import           Dungeon.Actor           (Actor, monster)
import           Dungeon.Actor.Status    (status)
import           Dungeon.Actor.Status.Hp (hp)
import           Localization            (multilingualText)

orc :: Coord -> Actor
orc c = monster c name st "images/orc.png"
    where st = status (hp 10) 0 3
          name = multilingualText "Orc" "オーク"

troll :: Coord -> Actor
troll c = monster c name st "images/troll.png"
    where st = status (hp 16) 1 4
          name = multilingualText "Troll" "トロール"
