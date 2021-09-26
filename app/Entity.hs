{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Entity
    ( Entity
    , position
    , char
    , name
    , entityAttr
    , player
    , orc
    , troll
    , isPlayer
    ) where

import           Brick.AttrMap (AttrName)
import           Control.Lens  (makeLenses, (^.))
import           Coord         (Coord)

data Entity = Actor
            { _position   :: Coord
            , _char       :: String
            , _entityAttr :: AttrName
            , _name       :: [Char]
            , _hp         :: Int
            , _maxHp      :: Int
            , _defence    :: Int
            , _power      :: Int
            } deriving (Show)
makeLenses ''Entity

player :: Coord -> Entity
player c = Actor { _position = c
                  , _char = "@"
                  , _entityAttr = "playerAttr"
                  , _name = "Player"
                  , _hp = 30
                  , _maxHp = 30
                  , _defence = 2
                  , _power = 5
                  }

orc :: Coord -> Entity
orc c = Actor { _position = c
               , _char = "o"
               , _entityAttr = "orcAttr"
               , _name = "Orc"
               , _hp = 10
               , _maxHp = 10
               , _defence = 0
               , _power = 3
               }

troll :: Coord -> Entity
troll c = Actor { _position = c
                 , _char = "T"
                 , _entityAttr = "trollAttr"
                 , _name = "Troll"
                 , _hp = 16
                 , _maxHp = 16
                 , _defence = 1
                 , _power = 4
                 }

isPlayer :: Entity -> Bool
isPlayer Actor { _name = name } = name == "Player"

getHp :: Entity -> Int
getHp e = e ^. hp
