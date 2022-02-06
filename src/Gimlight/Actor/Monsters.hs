{-# LANGUAGE OverloadedStrings #-}

module Gimlight.Actor.Monsters
    ( orc
    , troll
    ) where

import           Control.Monad.State       (State)
import           Gimlight.Actor            (Actor, monster)
import           Gimlight.Actor.Identifier (Identifier (Orc, Troll))
import           Gimlight.Actor.Status     (status)
import           Gimlight.Actor.Status.Hp  (hp)
import           Gimlight.IndexGenerator   (IndexGenerator)

orc :: State IndexGenerator Actor
orc = monster Orc st "images/orc.png"
  where
    st = status (hp 10) 0 3

troll :: State IndexGenerator Actor
troll = monster Troll st "images/troll.png"
  where
    st = status (hp 16) 1 4
