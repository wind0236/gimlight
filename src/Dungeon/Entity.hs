{-# LANGUAGE OverloadedStrings #-}
module Dungeon.Entity
    ( Entity
    , player
    , getHp
    , updateHp
    , monster
    , isPlayer
    , isMonster
    , isActor
    ) where

import           Control.Lens  ((&), (.~), (^.))
import           Coord         (Coord)
import           Data.Text     (Text)
import           Dungeon.Types (ActorKind (Monster, Player), Entity (Actor),
                                actor, actorKind, hp, maxHp)

monster :: Coord -> Text -> Int -> Int -> Int -> Text -> Entity
monster position name' maxHp' defence power walking = actor position name' maxHp' defence power True Monster "" walking "images/sample_standing_picture.png"

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
