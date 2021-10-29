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
    where st = status h 0 3
          h = case hp 10 of
                  Just x  -> x
                  Nothing -> error "Unreachable as the value is positive."
          name = multilingualText "Orc" "オーク"

troll :: Coord -> Actor
troll c = monster c name st "images/troll.png"
    where st = status h 1 4
          h = case hp 16 of
                  Just x  -> x
                  Nothing -> error "Unreachable as the value is positive."
          name = multilingualText "Troll" "トロール"
