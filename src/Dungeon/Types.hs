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
    , DungeonKind(..)
    , Entity(Actor)
    , pathToDestination
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
    , isAlive
    , blocksMovement
    , talkMessage
    , walkingImagePath
    , standingImagePath
    , positionOnGlobalMap
    , dungeonKind
    , actorKind
    , ActorKind(..)
    ) where

import           Control.Lens         (makeLenses)
import           Coord                (Coord)
import           Data.Array           (bounds)
import           Data.Binary          (Binary)
import           Data.Text            (Text)
import           Dungeon.Map.Explored (ExploredMap, initExploredMap)
import           Dungeon.Map.Fov      (Fov, initFov)
import           Dungeon.Map.Tile     (TileMap)
import           GHC.Generics         (Generic)
import           Linear.V2            (V2 (V2))


data ActorKind = Player | FriendlyNpc | Monster deriving (Show, Ord, Eq, Generic)
instance Binary ActorKind

data Entity = Actor
            { _position          :: Coord
            , _name              :: Text
            , _hp                :: Int
            , _maxHp             :: Int
            , _defence           :: Int
            , _power             :: Int
            , _pathToDestination :: [Coord]
            , _isAlive           :: Bool
            , _blocksMovement    :: Bool
            , _actorKind         :: ActorKind
            , _talkMessage       :: Text
            , _walkingImagePath  :: Text
            , _standingImagePath :: Text
            } deriving (Show, Ord, Eq, Generic)
makeLenses ''Entity
instance Binary Entity

data DungeonKind = Town | DungeonType | GlobalMap deriving (Show, Ord, Eq, Generic)
instance Binary DungeonKind

data Dungeon = Dungeon
          { _tileMap             :: TileMap
          , _visible             :: Fov
          , _explored            :: ExploredMap
          , _entities            :: [Entity]
          , _positionOnGlobalMap :: Maybe Coord
          , _dungeonKind         :: DungeonKind
          } deriving (Show, Ord, Eq, Generic)
makeLenses ''Dungeon
instance Binary Dungeon

dungeon :: TileMap -> [Entity] -> Maybe Coord -> DungeonKind -> Dungeon
dungeon t e p d = Dungeon { _tileMap = t
                          , _visible = initFov widthAndHeight
                          , _explored = initExploredMap widthAndHeight
                          , _entities = e
                          , _positionOnGlobalMap = p
                          , _dungeonKind = d
                          }
    where widthAndHeight = snd (bounds t) + V2 1 1

actor :: Coord -> Text -> Int -> Int -> Int -> Bool -> Bool -> ActorKind -> Text -> Text -> Text -> Entity
actor position' name' hp' defence' power' isAlive' blocksMovement' ak talkMessage' walkingImagePath' standingImagePath'=
        Actor { _position = position'
              , _name = name'
              , _hp = hp'
              , _maxHp = hp'
              , _defence = defence'
              , _power = power'
              , _pathToDestination = []
              , _isAlive = isAlive'
              , _blocksMovement = blocksMovement'
              , _talkMessage = talkMessage'
              , _walkingImagePath = walkingImagePath'
              , _standingImagePath = standingImagePath'
              , _actorKind = ak
              }
