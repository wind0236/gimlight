{-# LANGUAGE DeriveGeneric #-}

module Dungeon.Map.Actor
    ( ActorMap
    , actorMap
    ) where

import           Actor        (Actor)
import           Coord        (Coord)
import           Data.Array   (Array, array)
import           GHC.Generics (Generic)
import           Linear.V2    (V2 (V2))

newtype ActorMap =
    ActorMap (Array Coord (Maybe Actor))
    deriving (Show, Ord, Eq, Generic)

actorMap :: V2 Int -> ActorMap
actorMap (V2 width height) =
    ActorMap $
    array
        (V2 0 0, V2 (width - 1) (height - 1))
        [(V2 x y, Nothing) | x <- [0 .. width - 1], y <- [0 .. height - 1]]
