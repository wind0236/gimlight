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

hp :: Int -> Hp
hp h = case hpOrFail h of
                 Just x  -> x
                 Nothing -> error "The initial HP value must be positive."

getHp :: Hp -> Int
getHp = currentHp

getMaxHp :: Hp -> Int
getMaxHp = maxHp

healHp :: Int -> Hp -> Hp
healHp amount Hp { currentHp = c, maxHp = m }
    = Hp { currentHp = min m $ c + amount, maxHp = m }

receiveDamage :: Int -> Hp -> Maybe Hp
receiveDamage damage Hp { currentHp = c, maxHp = m }
    = if newHp > 0 then Just $ Hp { currentHp = newHp, maxHp = m } else Nothing
    where newHp = c - damage

hpOrFail :: Int -> Maybe Hp
hpOrFail h
    | h > 0 = Just $ Hp h h
    | otherwise = Nothing
