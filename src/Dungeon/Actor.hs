{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Dungeon.Actor
    ( Actor
    , player
    , getLevel
    , getCurrentExperiencePoint
    , getExperiencePointForNextLevel
    , getHp
    , getMaxHp
    , getPower
    , getDefence
    , monster
    , isPlayer
    , isMonster
    , ActorKind(FriendlyNpc)
    , attackFromTo
    , actor
    , position
    , name
    , pathToDestination
    , standingImagePath
    , talkMessage
    , walkingImagePath
    , healHp
    , inventoryItems
    , getItems
    , removeNthItem
    ) where

import           Control.Lens            (makeLenses, (%~), (&), (.~), (^.))
import           Control.Monad.Writer    (MonadWriter (writer), Writer)
import           Coord                   (Coord)
import           Data.Binary             (Binary)
import           Data.Text               (Text)
import           Dungeon.Actor.Inventory (Inventory, inventory)
import qualified Dungeon.Actor.Inventory as I
import           Dungeon.Actor.Status    (Status)
import qualified Dungeon.Actor.Status    as S
import           Dungeon.Actor.Status.Hp (hp)
import           Dungeon.Item            (Item)
import           GHC.Generics            (Generic)
import           Localization            (MultilingualText)
import qualified Localization.Texts      as T
import           Log                     (MessageLog)

data ActorKind = Player | FriendlyNpc | Monster deriving (Show, Ord, Eq, Generic)
instance Binary ActorKind

data Actor = Actor
           { _position          :: Coord
           , _name              :: MultilingualText
           , _status            :: Status
           , _pathToDestination :: [Coord]
           , _actorKind         :: ActorKind
           , _talkMessage       :: MultilingualText
           , _walkingImagePath  :: Text
           , _standingImagePath :: Text
           , _inventoryItems    :: Inventory
           } deriving (Show, Ord, Eq, Generic)
makeLenses ''Actor
instance Binary Actor

actor :: Coord -> MultilingualText -> Status -> ActorKind -> MultilingualText -> Text -> Text -> Actor
actor position' name' st ak talkMessage' walkingImagePath' standingImagePath'=
    Actor { _position = position'
          , _name = name'
          , _status = st
          , _pathToDestination = []
          , _talkMessage = talkMessage'
          , _walkingImagePath = walkingImagePath'
          , _standingImagePath = standingImagePath'
          , _actorKind = ak
          , _inventoryItems = inventory 5
          }

monster :: Coord -> MultilingualText -> Status -> Text -> Actor
monster position' name' st walking = actor position' name' st Monster mempty walking "images/sample_standing_picture.png"

player :: Coord -> Actor
player c = actor c T.player st Player mempty "images/player.png" "images/sample_standing_picture.png"
    where st = S.status (hp 30) 5 2

isPlayer :: Actor -> Bool
isPlayer e = (e ^. actorKind) == Player

isMonster :: Actor -> Bool
isMonster e = (e ^. actorKind) == Monster

getHp :: Actor -> Int
getHp e = S.getHp $ e ^. status

getMaxHp :: Actor -> Int
getMaxHp e = S.getMaxHp $ e ^. status

attackFromTo :: Actor -> Actor -> (Writer MessageLog) (Actor, Maybe Actor)
attackFromTo attacker defender = writer ((newAttacker, newDefender), msg)
    where (newAttackerStatus, newDefenderStatus, msgFunc) = S.attackFromTo (attacker ^. status) (defender ^. status)
          newAttacker = attacker & status .~ newAttackerStatus
          newDefender = (\x -> defender & status .~ x) <$> newDefenderStatus
          msg = msgFunc (attacker ^. name) (defender ^. name)

healHp :: Int -> Actor -> Actor
healHp amount a = a & status %~ S.healHp amount

getLevel :: Actor -> Int
getLevel a = S.getLevel $ a ^. status

getCurrentExperiencePoint :: Actor -> Int
getCurrentExperiencePoint a = S.getCurrentExperiencePoint $ a ^. status

getExperiencePointForNextLevel :: Actor -> Int
getExperiencePointForNextLevel a = S.getExperiencePointForNextLevel $ a ^. status

getPower :: Actor -> Int
getPower a = S.getPower $ a ^. status

getDefence :: Actor -> Int
getDefence a = S.getDefence $ a ^. status

getItems :: Actor -> [Item]
getItems a = I.getItems $ a ^. inventoryItems

removeNthItem :: Int -> Actor -> (Maybe Item, Actor)
removeNthItem n a = (removed, a & inventoryItems .~ newItems)
    where (removed, newItems) = I.removeNthItem n $ a ^. inventoryItems
