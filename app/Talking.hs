{-# LANGUAGE TemplateHaskell #-}

module Talking
    ( TalkWith
    , talkWith
    , person
    , message
    ) where

import           Control.Lens  (makeLenses)
import           Dungeon.Types (Entity)

data TalkWith = TalkWith
              { _person  :: Entity
              , _message :: String
              } deriving (Show, Ord, Eq)
makeLenses ''TalkWith

talkWith :: Entity -> String -> TalkWith
talkWith = TalkWith
