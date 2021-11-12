{-# LANGUAGE OverloadedStrings #-}

module Actor.Friendly.Electria
    ( electria
    ) where

import           Actor              (Actor)
import           Actor.Friendly     (friendly)
import           Actor.Status       (status)
import           Actor.Status.Hp    (hp)
import           Coord              (Coord)
import qualified Localization.Texts as T

electria :: Coord -> Actor
electria position =
    friendly
        position
        T.electria
        st
        T.talkWithElectria
        "images/electria.png"
        "images/sample_standing_picture.png"
  where
    st = status (hp 1) 1 1
