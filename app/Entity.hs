{-# LANGUAGE TemplateHaskell #-}

module Entity
    ( Entity
    , player
    , getHp
    , updateHp
    , monster
    ) where

import           Brick.AttrMap (AttrName)
import           Control.Lens  (makeLenses, (&), (&~), (.=), (.~), (^.))
import           Coord         (Coord)
import           Dungeon.Types (Entity, RenderOrder (ActorEntity, Corpse),
                                actor, blocksMovement, char, entityAttr, hp,
                                isAlive, maxHp, name, renderOrder)
import           UI.Attrs      (greenAttr, redAttr, whiteAttr)

monster :: Coord -> String -> AttrName -> String -> Int -> Int -> Int -> Entity
monster position char entityAttr name maxHp defence power =
        actor position char entityAttr name maxHp defence power True True False ActorEntity True

player :: Coord -> Entity
player c = actor c "@" whiteAttr "Player" 30 2 5 True True True ActorEntity False

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
    char .= "%"
    entityAttr .= redAttr
    blocksMovement .= False
    name .= "remains of " ++ (e ^. name)
    isAlive .= False
    renderOrder .= Corpse
