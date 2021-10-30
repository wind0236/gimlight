{-# LANGUAGE DeriveGeneric #-}

module GameModel.Status.Talking
    ( TalkingHandler
    , talkingHandler
    , destructHandler
    , finishTalking
    ) where

import           Data.Binary                (Binary)
import           GHC.Generics               (Generic)
import           GameModel.Status.Exploring (ExploringHandler)
import           Talking                    (TalkWith)

data TalkingHandler = TalkingHandler
                    { talk         :: TalkWith
                    , afterTalking :: ExploringHandler
                    } deriving (Show, Ord, Eq, Generic)

instance Binary TalkingHandler

talkingHandler :: TalkWith -> ExploringHandler -> TalkingHandler
talkingHandler = TalkingHandler

destructHandler :: TalkingHandler -> (TalkWith, ExploringHandler)
destructHandler TalkingHandler { talk = t, afterTalking = at } = (t, at)

finishTalking :: TalkingHandler -> ExploringHandler
finishTalking TalkingHandler { afterTalking = at } = at
