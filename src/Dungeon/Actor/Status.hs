{-# LANGUAGE DeriveGeneric #-}

module Dungeon.Actor.Status
    ( Status
    , status
    , getHp
    , getMaxHp
    , healHp
    , receiveDamage
    , getPower
    , getDefence
    ) where

import           Data.Binary             (Binary)
import           Dungeon.Actor.Status.Hp (Hp)
import qualified Dungeon.Actor.Status.Hp as HP
import           GHC.Generics            (Generic)

data Status = Status
            { hp      :: Hp
            , power   :: Int
            , defence :: Int
            } deriving (Show, Ord, Eq, Generic)

instance Binary Status

status :: Hp -> Int -> Int -> Status
status = Status

getHp :: Status -> Int
getHp Status { hp = h }= HP.getHp h

getMaxHp :: Status -> Int
getMaxHp Status { hp = h }= HP.getMaxHp h

healHp :: Int -> Status -> Status
healHp amount a@Status { hp = h } =
    a { hp = HP.healHp amount h }

receiveDamage :: Int -> Status -> Maybe Status
receiveDamage damage a@Status { hp = h } =
    (\x -> a { hp = x }) <$> HP.receiveDamage damage h

getPower :: Status -> Int
getPower = power

getDefence :: Status -> Int
getDefence = defence
