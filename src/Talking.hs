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
import           Dungeon.Actor (Actor)
import           GHC.Generics  (Generic)
import           Localization  (MultilingualText)

data TalkWith = TalkWith
              { _person  :: Actor
              , _message :: MultilingualText
              } deriving (Show, Ord, Eq, Generic)
makeLenses ''TalkWith
instance Binary TalkWith

talkWith :: Actor -> MultilingualText -> TalkWith
talkWith = TalkWith
