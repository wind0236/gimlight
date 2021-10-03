module Entity.Friendly
    ( electria
    ) where

import           Brick.AttrMap (AttrName)
import           Coord         (Coord)
import           Dungeon.Types (RenderOrder (ActorEntity), actor)
import           Entity        (Entity)
import           UI.Attrs      (redAttr)

electria :: Coord -> Entity
electria position = friendly position "1" redAttr "Electria" 50 50 50

friendly :: Coord -> String -> AttrName -> String -> Int -> Int -> Int -> Entity
friendly position char entityAttr name maxHp defence power =
        actor position char entityAttr name maxHp defence power True True False ActorEntity False
