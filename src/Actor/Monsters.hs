{-# LANGUAGE OverloadedStrings #-}

module Actor.Monsters
    ( orc
    , troll
    ) where

import           Actor              (Actor, monster)
import           Actor.Status       (status)
import           Actor.Status.Hp    (hp)
import           Coord              (Coord)
import qualified Localization.Texts as T

orc :: Coord -> Actor
orc c = monster c T.orc st "images/orc.png"
  where
    st = status (hp 10) 0 3

troll :: Coord -> Actor
troll c = monster c T.troll st "images/troll.png"
  where
    st = status (hp 16) 1 4
