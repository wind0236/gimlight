module Dungeon.Entity.Friendly
    ( electria
    ) where

import           Coord          (Coord)
import           Dungeon.Entity (Entity)
import           Dungeon.Types  (actor)

electria :: Coord -> Entity
electria position = friendly position "Electria" 50 50 50 "How's it going, Ruskell?" "images/electria.png"

friendly :: Coord -> String -> Int -> Int -> Int -> String -> String -> Entity
friendly position name maxHp defence power = actor position name maxHp defence power True True False False
