{-# LANGUAGE DeriveGeneric #-}

module Dungeon.Actor.Status.Hp
    ( Hp
    , hp
    , getHp
    , getMaxHp
    , healHp
    , receiveDamage
    ) where

import           Data.Binary  (Binary)
import           GHC.Generics (Generic)

data Hp = Hp
        { currentHp :: Int
        , maxHp     :: Int
        } deriving (Show, Ord, Eq, Generic)

instance Binary Hp

hp :: Int -> Maybe Hp
hp h
    | h > 0 = Just $ Hp h h
    | otherwise = Nothing

getHp :: Hp -> Int
getHp = currentHp

getMaxHp :: Hp -> Int
getMaxHp = maxHp

healHp :: Int -> Hp -> Hp
healHp amount Hp { currentHp = c, maxHp = m }
    = Hp { currentHp = min m $ c + amount, maxHp = m }

receiveDamage :: Int -> Hp -> Hp
receiveDamage damage Hp { currentHp = c, maxHp = m }
    = Hp { currentHp = max 0 $ c - damage, maxHp = m }
