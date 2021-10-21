{-# LANGUAGE OverloadedStrings #-}

module Dungeon.Actor.Monsters
    ( orc
    , troll
    ) where
import           Coord         (Coord)
import           Dungeon.Actor (Actor, monster)
import           Localization  (multilingualText)

orc :: Coord -> Actor
orc c = monster c name 10 0 3 "images/orc.png"
    where name = multilingualText "Orc" "オーク"

troll :: Coord -> Actor
troll c = monster c name 16 1 4 "images/troll.png"
    where name = multilingualText "Troll" "トロール"
