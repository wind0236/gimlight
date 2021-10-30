{-# LANGUAGE OverloadedStrings #-}

module Dungeon.Actor.Actions.Move
    ( moveAction
    ) where

import           Control.Lens          ((&), (.~), (^.))
import           Control.Monad.Writer  (MonadPlus (mzero), tell)
import           Coord                 (Coord)
import           Data.Array            ((!))
import           Data.Maybe            (isNothing)
import           Dungeon               (Dungeon, actorAt, mapWidthAndHeight,
                                        pushActor, tileMap)
import           Dungeon.Actor         (Actor, position)
import           Dungeon.Actor.Actions (Action)
import           Dungeon.Map.Tile      (walkable)
import           Linear.V2             (V2 (V2))
import qualified Localization.Texts    as T

moveAction :: V2 Int -> Action
moveAction offset src d = if not (movable d (src ^. position + offset))
                                then do
                                    tell [T.youCannotMoveThere]
                                    mzero
                                else return $ pushActor (updatePosition d src offset) d

updatePosition :: Dungeon -> Actor -> V2 Int -> Actor
updatePosition d src offset
    = let next = nextPosition d src offset
      in if movable d next
            then src & position .~ next
            else src

movable :: Dungeon -> Coord -> Bool
movable d c = isNothing (actorAt c d) && isPositionInRange d c && (d ^. tileMap) ! c ^. walkable

nextPosition :: Dungeon -> Actor -> V2 Int -> Coord
nextPosition d src offset =
    max (V2 0 0) $ min (V2 (width - 1) $ height - 1) $ src ^. position + offset
    where V2 width height = mapWidthAndHeight d

isPositionInRange :: Dungeon -> Coord -> Bool
isPositionInRange d c = x >= 0 && x < width && y >= 0 && y < height
    where V2 width height = mapWidthAndHeight d
          V2 x y = c
