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
import           Data.Text     (Text)
import           Dungeon.Actor (Actor)
import           GHC.Generics  (Generic)

data TalkWith = TalkWith
              { _person  :: Actor
              , _message :: Text
              } deriving (Show, Ord, Eq, Generic)
makeLenses ''TalkWith
instance Binary TalkWith

talkWith :: Actor -> Text -> TalkWith
talkWith = TalkWith
