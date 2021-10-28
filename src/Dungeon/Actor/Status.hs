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

import           Data.Binary  (Binary)
import           GHC.Generics (Generic)

data Status = Status
            { hp      :: Int
            , maxHp   :: Int
            , power   :: Int
            , defence :: Int
            } deriving (Show, Ord, Eq, Generic)

instance Binary Status

status :: Int -> Int -> Int -> Status
status initHp = Status initHp initHp

getHp :: Status -> Int
getHp = hp

getMaxHp :: Status -> Int
getMaxHp = maxHp

healHp :: Int -> Status -> Status
healHp amount a = updateHp (getHp a + amount) a

receiveDamage :: Int -> Status -> Status
receiveDamage damage a = updateHp (getHp a - damage) a

getPower :: Status -> Int
getPower = power

getDefence :: Status -> Int
getDefence = defence

updateHp :: Int -> Status -> Status
updateHp newHp s
    = s { hp = newHpInRange }
    where newHpInRange = max 0 $ min (getMaxHp s) newHp
