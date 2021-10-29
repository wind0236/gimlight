{-# LANGUAGE OverloadedStrings #-}

module Dungeon.Actor.Monsters
    ( orc
    , troll
    ) where
import           Coord         (Coord)
import           Dungeon.Actor (Actor, monster)
import           Localization  (multilingualText)

orc :: Coord -> Actor
orc c = case m of
            Just x  -> x
            Nothing -> error "Failed to create the orc actor."
    where m = monster c name 10 0 3 "images/orc.png"
          name = multilingualText "Orc" "オーク"

troll :: Coord -> Actor
troll c = case m of
              Just x  -> x
              Nothing -> error "Failed to create the troll actor."
    where m = monster c name 16 1 4 "images/troll.png"
          name = multilingualText "Troll" "トロール"
