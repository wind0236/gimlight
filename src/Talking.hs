{-# LANGUAGE DeriveGeneric #-}

module Talking
    ( TalkWith
    , talkWith
    , getPerson
    , getMessage
    ) where

import           Actor        (Actor)
import           Data.Binary  (Binary)
import           GHC.Generics (Generic)
import           Localization (MultilingualText)

data TalkWith =
    TalkWith
        { person  :: Actor
        , message :: MultilingualText
        }
    deriving (Show, Ord, Eq, Generic)

instance Binary TalkWith

talkWith :: Actor -> MultilingualText -> TalkWith
talkWith = TalkWith

getPerson :: TalkWith -> Actor
getPerson = person

getMessage :: TalkWith -> MultilingualText
getMessage = message
