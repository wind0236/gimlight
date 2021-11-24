{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Actor
    ( Actor
    , player
    , getIdentifier
    , getLevel
    , getCurrentExperiencePoint
    , getExperiencePointForNextLevel
    , getHp
    , getMaxHp
    , getPower
    , getDefence
    , getTalkingPart
    , monster
    , isPlayer
    , isMonster
    , isFriendlyNpc
    , ActorKind(FriendlyNpc)
    , attackFromTo
    , actor
    , position
    , pathToDestination
    , standingImagePath
    , walkingImagePath
    , healHp
    , inventoryItems
    , getItems
    , removeNthItem
    ) where

import           Actor.Identifier        (Identifier, toName)
import qualified Actor.Identifier        as Identifier
import           Actor.Inventory         (Inventory, inventory)
import qualified Actor.Inventory         as I
import           Actor.Status            (Status)
import qualified Actor.Status            as S
import           Actor.Status.Hp         (hp)
import           Control.Lens            (makeLenses, (%~), (&), (.~), (^.))
import           Control.Monad.Writer    (MonadWriter (writer), Writer)
import           Coord                   (Coord)
import           Data.Binary             (Binary)
import           Data.Text               (Text)
import           GHC.Generics            (Generic)
import           GameStatus.Talking.Part (TalkingPart)
import           Item                    (Item)
import           Log                     (MessageLog)

data ActorKind
    = Player
    | FriendlyNpc
    | Monster
    deriving (Show, Ord, Eq, Generic)

instance Binary ActorKind

data Actor =
    Actor
        { _position          :: Coord
        , _identifier        :: Identifier
        , _status            :: Status
        , _pathToDestination :: [Coord]
        , _actorKind         :: ActorKind
        , _talk              :: Maybe TalkingPart
        , _walkingImagePath  :: Text
        , _standingImagePath :: Text
        , _inventoryItems    :: Inventory
        }
    deriving (Show, Ord, Eq, Generic)

makeLenses ''Actor

instance Binary Actor

actor ::
       Coord
    -> Identifier
    -> Status
    -> ActorKind
    -> Maybe TalkingPart
    -> Text
    -> Text
    -> Actor
actor position' id' st ak talkMessage' walkingImagePath' standingImagePath' =
    Actor
        { _position = position'
        , _identifier = id'
        , _status = st
        , _pathToDestination = []
        , _talk = talkMessage'
        , _walkingImagePath = walkingImagePath'
        , _standingImagePath = standingImagePath'
        , _actorKind = ak
        , _inventoryItems = inventory 5
        }

monster :: Coord -> Identifier -> Status -> Text -> Actor
monster position' name' st walking =
    actor
        position'
        name'
        st
        Monster
        Nothing
        walking
        "images/sample_standing_picture.png"

player :: Coord -> Actor
player c =
    actor
        c
        Identifier.Player
        st
        Player
        Nothing
        "images/player.png"
        "images/sample_standing_picture.png"
  where
    st = S.status (hp 30) 5 2

isPlayer :: Actor -> Bool
isPlayer e = e ^. actorKind == Player

isMonster :: Actor -> Bool
isMonster e = e ^. actorKind == Monster

isFriendlyNpc :: Actor -> Bool
isFriendlyNpc e = e ^. actorKind == FriendlyNpc

getIdentifier :: Actor -> Identifier
getIdentifier a = a ^. identifier

getHp :: Actor -> Int
getHp e = S.getHp $ e ^. status

getMaxHp :: Actor -> Int
getMaxHp e = S.getMaxHp $ e ^. status

getTalkingPart :: Actor -> Maybe TalkingPart
getTalkingPart a = a ^. talk

attackFromTo :: Actor -> Actor -> (Writer MessageLog) (Actor, Maybe Actor)
attackFromTo attacker defender = writer ((newAttacker, newDefender), msg)
  where
    (newAttackerStatus, newDefenderStatus, msgFunc) =
        S.attackFromTo (attacker ^. status) (defender ^. status)
    newAttacker = attacker & status .~ newAttackerStatus
    newDefender = (\x -> defender & status .~ x) <$> newDefenderStatus
    msg =
        msgFunc
            (toName $ getIdentifier attacker)
            (toName $ getIdentifier defender)

healHp :: Int -> Actor -> Actor
healHp amount a = a & status %~ S.healHp amount

getLevel :: Actor -> Int
getLevel a = S.getLevel $ a ^. status

getCurrentExperiencePoint :: Actor -> Int
getCurrentExperiencePoint a = S.getCurrentExperiencePoint $ a ^. status

getExperiencePointForNextLevel :: Actor -> Int
getExperiencePointForNextLevel a =
    S.getExperiencePointForNextLevel $ a ^. status

getPower :: Actor -> Int
getPower a = S.getPower $ a ^. status

getDefence :: Actor -> Int
getDefence a = S.getDefence $ a ^. status

getItems :: Actor -> [Item]
getItems a = I.getItems $ a ^. inventoryItems

removeNthItem :: Int -> Actor -> (Maybe Item, Actor)
removeNthItem n a = (removed, a & inventoryItems .~ newItems)
  where
    (removed, newItems) = I.removeNthItem n $ a ^. inventoryItems
