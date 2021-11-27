{-# LANGUAGE OverloadedStrings #-}

module Actor.Monsters
    ( orc
    , troll
    ) where

import           Actor            (Actor, monster)
import           Actor.Identifier (Identifier (Orc, Troll))
import           Actor.Status     (status)
import           Actor.Status.Hp  (hp)
import           IndexGenerator   (IndexGenerator)

orc :: IndexGenerator -> (Actor, IndexGenerator)
orc ig = monster ig Orc st "images/orc.png"
  where
    st = status (hp 10) 0 3

troll :: IndexGenerator -> (Actor, IndexGenerator)
troll ig = monster ig Troll st "images/troll.png"
  where
    st = status (hp 16) 1 4
