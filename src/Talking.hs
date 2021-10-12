{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Talking
    ( TalkWith
    , talkWith
    , person
    , message
    ) where

import           Control.Lens  (makeLenses)
import           Data.Binary   (Binary)
import           Dungeon.Types (Entity)
import           GHC.Generics  (Generic)

data TalkWith = TalkWith
              { _person  :: Entity
              , _message :: String
              } deriving (Show, Ord, Eq, Generic)
makeLenses ''TalkWith
instance Binary TalkWith

talkWith :: Entity -> String -> TalkWith
talkWith = TalkWith
