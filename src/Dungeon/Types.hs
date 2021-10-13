-- This module is a workaround to avoid circular dependencies between
-- `Dungeon` and `Entity`. The actual logics are defined in the `Dungeon`
-- and `Entity` modules.
--
-- This module should define only the types and constructors.
--
-- TODO: Define these types in each module if possible.

{-# LANGUAGE DeriveGeneric   #-}
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
    , walkingImagePath
    , standingImagePath
    , positionOnGlobalMap
    , isGlobalMap
    ) where

import           Control.Lens         (makeLenses)
import           Coord                (Coord)
import           Data.Array           (bounds)
import           Data.Binary          (Binary)
import           Dungeon.Map.Explored (ExploredMap, initExploredMap)
import           Dungeon.Map.Fov      (Fov, initFov)
import           Dungeon.Map.Tile     (TileMap)
import           GHC.Generics         (Generic)
import           Linear.V2            (V2 (V2))


newtype Ai = HostileEnemy
             { _path :: [Coord]
             } deriving (Show, Ord, Eq, Generic)
makeLenses ''Ai
instance Binary Ai

data Entity = Actor
            { _position          :: Coord
            , _name              :: String
            , _hp                :: Int
            , _maxHp             :: Int
            , _defence           :: Int
            , _power             :: Int
            , _ai                :: Ai
            , _isAlive           :: Bool
            , _blocksMovement    :: Bool
            , _isPlayer          :: Bool
            , _isEnemy           :: Bool
            , _talkMessage       :: String
            , _walkingImagePath  :: String
            , _standingImagePath :: String
            } deriving (Show, Ord, Eq, Generic)
makeLenses ''Entity
instance Binary Entity

data Dungeon = Dungeon
          { _tileMap             :: TileMap
          , _visible             :: Fov
          , _explored            :: ExploredMap
          , _entities            :: [Entity]
          , _positionOnGlobalMap :: Maybe Coord
          , _isGlobalMap         :: Bool
          } deriving (Show, Ord, Eq, Generic)
makeLenses ''Dungeon
instance Binary Dungeon

dungeon :: TileMap -> [Entity] -> Maybe Coord -> Bool -> Dungeon
dungeon t e p i = Dungeon { _tileMap = t
                      , _visible = initFov widthAndHeight
                      , _explored = initExploredMap widthAndHeight
                      , _entities = e
                      , _positionOnGlobalMap = p
                      , _isGlobalMap = i
                      }
    where widthAndHeight = snd (bounds t) + V2 1 1

actor :: Coord -> String -> Int -> Int -> Int -> Bool -> Bool -> Bool -> Bool -> String -> String -> String -> Entity
actor position' name' hp' defence' power' isAlive' blocksMovement' isPlayer' isEnemy' talkMessage' walkingImagePath' standingImagePath'=
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
              , _walkingImagePath = walkingImagePath'
              , _standingImagePath = standingImagePath'
              }

hostileEnemy :: Ai
hostileEnemy = HostileEnemy { _path = [] }
