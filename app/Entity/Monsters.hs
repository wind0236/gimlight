module Entity.Monsters
    ( orc
    , troll
    ) where
import           Coord    (Coord)
import           Entity   (Entity (..), monster)
import           UI.Attrs (greenAttr)

orc :: Coord -> Entity
orc c = monster c "o" greenAttr "Orc" 10 0 3

troll :: Coord -> Entity
troll c = monster c "T" greenAttr "Troll" 16 1 4
