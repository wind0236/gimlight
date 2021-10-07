-- This module is a workaround to avoid circular dependencies between
-- `Dungeon` and `Entity`. The actual logics are defined in the `Dungeon`
-- and `Entity` modules.
--
-- This module should define only the types and constructors.
--
-- TODO: Define these types in each module if possible.

{-# LANGUAGE TemplateHaskell #-}

module Dungeon.Types
    ( Dungeon
    , Entity
    , Ai(..)
    , path
    , dungeon
    , actor
    , tileMap
    , visible
    , explored
    , entities
    , position
    , name
    , hp
    , maxHp
    , defence
    , power
    , ai
    , isAlive
    , blocksMovement
    , isPlayer
    , isEnemy
    , talkMessage
    , imagePath
    ) where

import           Control.Lens         (makeLenses)
import           Coord                (Coord)
import           Dungeon.Map.Explored (ExploredMap, initExploredMap)
import           Dungeon.Map.Fov      (Fov, initFov)
import           Dungeon.Map.Tile     (TileMap)


newtype Ai = HostileEnemy
             { _path :: [Coord]
             } deriving (Show, Ord, Eq)
makeLenses ''Ai

data Entity = Actor
            { _position       :: Coord
            , _name           :: String
            , _hp             :: Int
            , _maxHp          :: Int
            , _defence        :: Int
            , _power          :: Int
            , _ai             :: Ai
            , _isAlive        :: Bool
            , _blocksMovement :: Bool
            , _isPlayer       :: Bool
            , _isEnemy        :: Bool
            , _talkMessage    :: String
            , _imagePath      :: String
            } deriving (Show, Ord, Eq)
makeLenses ''Entity

data Dungeon = Dungeon
          { _tileMap  :: TileMap
          , _visible  :: Fov
          , _explored :: ExploredMap
          , _entities :: [Entity]
          } deriving (Show, Ord, Eq)
makeLenses ''Dungeon

dungeon :: TileMap -> [Entity] -> Dungeon
dungeon t e = Dungeon { _tileMap = t
                      , _visible = initFov
                      , _explored = initExploredMap
                      , _entities = e
                      }

actor :: Coord -> String -> Int -> Int -> Int -> Bool -> Bool -> Bool -> Bool -> String -> String -> Entity
actor position' name' hp' defence' power' isAlive' blocksMovement' isPlayer' isEnemy' talkMessage' imagePath' =
        Actor { _position = position'
              , _name = name'
              , _hp = hp'
              , _maxHp = hp'
              , _defence = defence'
              , _power = power'
              , _ai = hostileEnemy
              , _isAlive = isAlive'
              , _blocksMovement = blocksMovement'
              , _isPlayer = isPlayer'
              , _isEnemy = isEnemy'
              , _talkMessage = talkMessage'
              , _imagePath = imagePath'
              }

hostileEnemy :: Ai
hostileEnemy = HostileEnemy { _path = [] }
