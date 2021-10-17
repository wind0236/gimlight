{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Dungeon.Entity
    ( Entity
    , player
    , getHp
    , updateHp
    , monster
    , isPlayer
    , isMonster
    , isActor
    , ActorKind(FriendlyNpc)
    , actor
    , position
    , blocksMovement
    , defence
    , name
    , pathToDestination
    , power
    , standingImagePath
    , talkMessage
    , walkingImagePath
    , maxHp
    ) where

import           Control.Lens (makeLenses, (&), (.~), (^.))
import           Coord        (Coord)
import           Data.Binary  (Binary)
import           Data.Text    (Text)
import           GHC.Generics (Generic)

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
            , _blocksMovement    :: Bool
            , _actorKind         :: ActorKind
            , _talkMessage       :: Text
            , _walkingImagePath  :: Text
            , _standingImagePath :: Text
            } deriving (Show, Ord, Eq, Generic)
makeLenses ''Entity
instance Binary Entity

actor :: Coord -> Text -> Int -> Int -> Int -> Bool -> ActorKind -> Text -> Text -> Text -> Entity
actor position' name' hp' defence' power' blocksMovement' ak talkMessage' walkingImagePath' standingImagePath'=
        Actor { _position = position'
              , _name = name'
              , _hp = hp'
              , _maxHp = hp'
              , _defence = defence'
              , _power = power'
              , _pathToDestination = []
              , _blocksMovement = blocksMovement'
              , _talkMessage = talkMessage'
              , _walkingImagePath = walkingImagePath'
              , _standingImagePath = standingImagePath'
              , _actorKind = ak
              }

monster :: Coord -> Text -> Int -> Int -> Int -> Text -> Entity
monster position' name' maxHp' defence' power' walking = actor position' name' maxHp' defence' power' True Monster "" walking "images/sample_standing_picture.png"

player :: Coord -> Entity
player c = actor c "Player" 30 2 5 True Player "" "images/player.png" "images/sample_standing_picture.png"

isActor :: Entity -> Bool
isActor Actor{} = True

isPlayer :: Entity -> Bool
isPlayer e = (e ^. actorKind) == Player

isMonster :: Entity -> Bool
isMonster e = (e ^. actorKind) == Monster

getHp :: Entity -> Int
getHp e = e ^. hp

updateHp :: Entity -> Int -> Entity
updateHp e newHp = e & hp .~ newHpInRange
    where newHpInRange = max 0 $ min (e ^. maxHp) newHp
