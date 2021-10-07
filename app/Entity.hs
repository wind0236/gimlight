module Entity
    ( Entity
    , player
    , getHp
    , updateHp
    , monster
    ) where

import           Control.Lens  ((&), (&~), (.=), (.~), (^.))
import           Coord         (Coord)
import           Dungeon.Types (Entity, actor, blocksMovement, hp, isAlive,
                                maxHp, name)

monster :: Coord -> String -> Int -> Int -> Int -> String -> Entity
monster position name' maxHp' defence power = actor position name' maxHp' defence power True True False True ""

player :: Coord -> Entity
player c = actor c "Player" 30 2 5 True True True False "" "images/player.png"

getHp :: Entity -> Int
getHp e = e ^. hp

updateHp :: Entity -> Int -> Entity
updateHp e newHp =
        let newHpInRange = max 0 $ min (e ^. maxHp) newHp
        in if newHpInRange == 0 && e ^. isAlive
               then die e
               else e & hp .~ newHpInRange

die :: Entity -> Entity
die e = e &~ do
    blocksMovement .= False
    name .= "remains of " ++ (e ^. name)
    isAlive .= False
