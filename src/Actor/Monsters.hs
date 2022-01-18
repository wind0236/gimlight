{-# LANGUAGE OverloadedStrings #-}

module Actor.Monsters
    ( orc
    , troll
    ) where

import           Actor               (Actor, monster)
import           Actor.Identifier    (Identifier (Orc, Troll))
import           Actor.Status        (status)
import           Actor.Status.Hp     (hp)
import           Control.Monad.State (State)
import           IndexGenerator      (IndexGenerator)

orc :: State IndexGenerator Actor
orc = monster Orc st "images/orc.png"
  where
    st = status (hp 10) 0 3

troll :: State IndexGenerator Actor
troll = monster Troll st "images/troll.png"
  where
    st = status (hp 16) 1 4
