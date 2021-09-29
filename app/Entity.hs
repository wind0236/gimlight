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
    , Ai(..)
    , ai
    , path
    , getHp
    , power
    , defence
    , updateHp
    , hp
    , isAlive
    , blocksMovement
    , renderOrder
    ) where

import           Brick.AttrMap (AttrName)
import           Control.Lens  (makeLenses, (^.))
import           Coord         (Coord)

newtype Ai = HostileEnemy
             { _path :: [Coord]
             } deriving (Show)
makeLenses ''Ai

data RenderOrder =  ActorEntity| Iten | Corpse deriving (Show, Ord, Eq)

data Entity = Actor
            { _position       :: Coord
            , _char           :: String
            , _entityAttr     :: AttrName
            , _name           :: [Char]
            , _hp             :: Int
            , _maxHp          :: Int
            , _defence        :: Int
            , _power          :: Int
            , _ai             :: Ai
            , _isAlive        :: Bool
            , _blocksMovement :: Bool
            , _isPlayer       :: Bool
            , _renderOrder    :: RenderOrder
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
                  , _ai = hostileEnemy
                  , _isAlive = True
                  , _blocksMovement = True
                  , _isPlayer = True
                  , _renderOrder = ActorEntity
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
               , _ai = hostileEnemy
               , _isAlive = True
               , _blocksMovement = True
               , _isPlayer = False
               , _renderOrder = ActorEntity
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
                 , _ai = hostileEnemy
                 , _isAlive = True
                 , _blocksMovement = True
                 , _isPlayer = False
                 , _renderOrder = ActorEntity
                 }

hostileEnemy :: Ai
hostileEnemy = HostileEnemy { _path = [] }

getHp :: Entity -> Int
getHp e = e ^. hp

updateHp :: Entity -> Int -> Entity
updateHp e@Actor{ _hp = hp, _maxHp = maxHp } newHp =
        let hpInRange = max 0 $ min maxHp newHp
        in if hpInRange == 0 && e ^. isAlive
               then  die e
               else e{ _hp = max 0 $ min maxHp newHp }
die :: Entity -> Entity
die e = e{ _hp = 0
         , _char = "%"
         , _entityAttr = "deadAttr"
         , _blocksMovement = False
         , _name = "remains of " ++ (e ^. name)
         , _isAlive = False
         , _renderOrder = Corpse
         }
