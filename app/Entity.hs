{-# LANGUAGE TemplateHaskell #-}

module Entity(Entity(..),position,char,entityAttr) where

import           Brick.AttrMap   (AttrName)
import           Control.Lens.TH (makeLenses)
import           Coord           (Coord)

data Entity = Entity
            { _position   :: Coord
            , _char       :: String
            , _entityAttr :: AttrName
            } deriving (Show)
makeLenses ''Entity
