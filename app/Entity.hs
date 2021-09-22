module Entity(Entity(..)) where

import           Brick.AttrMap (AttrName)
import           Coord         (Coord)

data Entity = Entity
            { _position   :: Coord
            , _char       :: String
            , _entityAttr :: AttrName
            } deriving (Show)
