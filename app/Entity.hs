{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Entity
    ( Entity(..)
    , position
    , char
    , entityAttr
    , playerEntity
    , orcEntity
    , trollEntity
    ) where

import           Brick.AttrMap   (AttrName)
import           Control.Lens.TH (makeLenses)
import           Coord           (Coord)

data Entity = Entity
            { _position   :: Coord
            , _char       :: String
            , _entityAttr :: AttrName
            , _name       :: [Char]
            } deriving (Show)
makeLenses ''Entity

playerEntity :: Coord -> Entity
playerEntity c = Entity { _position = c
                        , _char = "@"
                        , _entityAttr = "playerAttr"
                        , _name = "Player"
                        }

orcEntity :: Coord -> Entity
orcEntity c = Entity { _position = c
                     , _char = "o"
                     , _entityAttr = "orcAttr"
                     , _name = "Orc"
                     }

trollEntity :: Coord -> Entity
trollEntity c = Entity { _position = c
                       , _char = "T"
                       , _entityAttr = "trollAttr"
                       , _name = "Troll"
                       }
