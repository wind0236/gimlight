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
    , RenderOrder(..)
    , Ai(..)
    , path
    , dungeon
    , actor
    , tileMap
    , visible
    , explored
    , entities
    , position
    , char
    , entityAttr
    , name
    , hp
    , maxHp
    , defence
    , power
    , ai
    , isAlive
    , blocksMovement
    , isPlayer
    , renderOrder
    , isEnemy
    ) where

import           Brick.AttrMap (AttrName)
import           Control.Lens  (makeLenses)
import           Coord         (Coord)
import           Map.Explored  (ExploredMap, initExploredMap)
import           Map.Fov       (Fov, initFov)
import           Map.Tile      (TileMap)


newtype Ai = HostileEnemy
             { _path :: [Coord]
             } deriving (Show)
makeLenses ''Ai

data RenderOrder =  ActorEntity| Iten | Corpse deriving (Show, Ord, Eq)

data Entity = Actor
            { _position       :: Coord
            , _char           :: String
            , _entityAttr     :: AttrName
            , _name           :: String
            , _hp             :: Int
            , _maxHp          :: Int
            , _defence        :: Int
            , _power          :: Int
            , _ai             :: Ai
            , _isAlive        :: Bool
            , _blocksMovement :: Bool
            , _isPlayer       :: Bool
            , _renderOrder    :: RenderOrder
            , _isEnemy        :: Bool
            } deriving (Show)
makeLenses ''Entity

data Dungeon = Dungeon
          { _tileMap  :: TileMap
          , _visible  :: Fov
          , _explored :: ExploredMap
          , _entities :: [Entity]
          } deriving (Show)
makeLenses ''Dungeon

dungeon :: TileMap -> [Entity] -> Dungeon
dungeon t e = Dungeon { _tileMap = t
                      , _visible = initFov
                      , _explored = initExploredMap
                      , _entities = e
                      }

actor :: Coord -> String -> AttrName -> String -> Int -> Int -> Int -> Bool -> Bool -> Bool -> RenderOrder -> Bool -> Entity
actor position char entityAttr name hp defence power isAlive blocksMovement isPlayer renderOrder isEnemy =
        Actor { _position = position
              , _char = char
              , _entityAttr = entityAttr
              , _name = name
              , _hp = hp
              , _maxHp = hp
              , _defence = defence
              , _power = power
              , _ai = hostileEnemy
              , _isAlive = isAlive
              , _blocksMovement = blocksMovement
              , _isPlayer = isPlayer
              , _renderOrder = renderOrder
              , _isEnemy = isEnemy
              }

hostileEnemy :: Ai
hostileEnemy = HostileEnemy { _path = [] }
