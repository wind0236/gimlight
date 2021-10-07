module Dungeon.Entity.Monsters
    ( orc
    , troll
    ) where
import           Coord          (Coord)
import           Dungeon.Entity (Entity, monster)

orc :: Coord -> Entity
orc c = monster c "Orc" 10 0 3 "images/orc.png"

troll :: Coord -> Entity
troll c = monster c "Troll" 16 1 4 "images/troll.png"
