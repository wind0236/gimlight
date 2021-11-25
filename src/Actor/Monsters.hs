{-# LANGUAGE OverloadedStrings #-}

module Actor.Monsters
    ( orc
    , troll
    ) where

import           Actor            (Actor, monster)
import           Actor.Identifier (Identifier (Orc, Troll))
import           Actor.Status     (status)
import           Actor.Status.Hp  (hp)
import           Coord            (Coord)
import           IndexGenerator   (IndexGenerator)

orc :: IndexGenerator -> Coord -> (Actor, IndexGenerator)
orc ig c = monster ig c Orc st "images/orc.png"
  where
    st = status (hp 10) 0 3

troll :: IndexGenerator -> Coord -> (Actor, IndexGenerator)
troll ig c = monster ig c Troll st "images/troll.png"
  where
    st = status (hp 16) 1 4
