{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Dungeon.Actor
    ( Actor
    , player
    , getHp
    , updateHp
    , monster
    , isPlayer
    , isMonster
    , ActorKind(FriendlyNpc)
    , actor
    , position
    , defence
    , name
    , pathToDestination
    , power
    , standingImagePath
    , talkMessage
    , walkingImagePath
    , maxHp
    , healHp
    , inventoryItems
    , getItems
    , removeNthItem
    ) where

import           Control.Lens            (makeLenses, (&), (.~), (^.))
import           Coord                   (Coord)
import           Data.Binary             (Binary)
import           Data.Text               (Text)
import           Dungeon.Actor.Inventory (Inventory, inventory)
import qualified Dungeon.Actor.Inventory as I
import           Dungeon.Item            (Item)
import           GHC.Generics            (Generic)

data ActorKind = Player | FriendlyNpc | Monster deriving (Show, Ord, Eq, Generic)
instance Binary ActorKind

data Actor = Actor
           { _position          :: Coord
           , _name              :: Text
           , _hp                :: Int
           , _maxHp             :: Int
           , _defence           :: Int
           , _power             :: Int
           , _pathToDestination :: [Coord]
           , _actorKind         :: ActorKind
           , _talkMessage       :: Text
           , _walkingImagePath  :: Text
           , _standingImagePath :: Text
           , _inventoryItems    :: Inventory
           } deriving (Show, Ord, Eq, Generic)
makeLenses ''Actor
instance Binary Actor

actor :: Coord -> Text -> Int -> Int -> Int -> ActorKind -> Text -> Text -> Text -> Actor
actor position' name' hp' defence' power' ak talkMessage' walkingImagePath' standingImagePath'=
        Actor { _position = position'
              , _name = name'
              , _hp = hp'
              , _maxHp = hp'
              , _defence = defence'
              , _power = power'
              , _pathToDestination = []
              , _talkMessage = talkMessage'
              , _walkingImagePath = walkingImagePath'
              , _standingImagePath = standingImagePath'
              , _actorKind = ak
              , _inventoryItems = inventory 5
              }

monster :: Coord -> Text -> Int -> Int -> Int -> Text -> Actor
monster position' name' maxHp' defence' power' walking = actor position' name' maxHp' defence' power' Monster "" walking "images/sample_standing_picture.png"

player :: Coord -> Actor
player c = actor c "Player" 30 2 5 Player "" "images/player.png" "images/sample_standing_picture.png"

isPlayer :: Actor -> Bool
isPlayer e = (e ^. actorKind) == Player

isMonster :: Actor -> Bool
isMonster e = (e ^. actorKind) == Monster

getHp :: Actor -> Int
getHp e = e ^. hp

updateHp :: Actor -> Int -> Actor
updateHp e newHp = e & hp .~ newHpInRange
    where newHpInRange = max 0 $ min (e ^. maxHp) newHp

healHp :: Actor -> Int -> Actor
healHp e amount = updateHp e $ getHp e + amount

getItems :: Actor -> [Item]
getItems a = I.getItems $ a ^. inventoryItems

removeNthItem :: Int -> Actor -> (Maybe Item, Actor)
removeNthItem n a = (removed, a & inventoryItems .~ newItems)
    where (removed, newItems) = I.removeNthItem n $ a ^. inventoryItems
