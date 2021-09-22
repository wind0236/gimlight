{-# LANGUAGE OverloadedStrings #-}

module Dungeon.Tile
    ( Tile(..)
    , floorTile
    , wallTile
    ) where

import           Brick.AttrMap (AttrName)

data Tile = Tile
          { _walkable    :: Bool
          , _transparent :: Bool
          , _darkAttr    :: AttrName
          , _lightAttr   :: AttrName
          } deriving (Show)

wallTile :: Tile
wallTile = Tile { _walkable = False
            , _transparent = False
            , _darkAttr = "darkWallAttr"
            , _lightAttr = "lightWallAttr"
            }

floorTile :: Tile
floorTile = Tile { _walkable = True
             , _transparent = True
             , _darkAttr = "darkFloorAttr"
             , _lightAttr = "lightFloorAttr"
             }
