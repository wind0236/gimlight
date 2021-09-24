{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Entity
    ( Entity(..)
    , position
    , char
    , name
    , entityAttr
    , player
    , orc
    , troll
    , isPlayer
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

player :: Coord -> Entity
player c = Entity { _position = c
                  , _char = "@"
                  , _entityAttr = "playerAttr"
                  , _name = "Player"
                  }

orc :: Coord -> Entity
orc c = Entity { _position = c
               , _char = "o"
               , _entityAttr = "orcAttr"
               , _name = "Orc"
               }

troll :: Coord -> Entity
troll c = Entity { _position = c
                 , _char = "T"
                 , _entityAttr = "trollAttr"
                 , _name = "Troll"
                 }

isPlayer :: Entity -> Bool
isPlayer Entity { _name = name } = name == "Player"
