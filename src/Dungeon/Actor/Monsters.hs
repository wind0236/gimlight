{-# LANGUAGE OverloadedStrings #-}

module Dungeon.Actor.Monsters
    ( orc
    , troll
    ) where
import           Coord         (Coord)
import           Dungeon.Actor (Actor, monster)

orc :: Coord -> Actor
orc c = monster c "Orc" 10 0 3 "images/orc.png"

troll :: Coord -> Actor
troll c = monster c "Troll" 16 1 4 "images/troll.png"
