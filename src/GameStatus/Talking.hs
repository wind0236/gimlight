{-# LANGUAGE DeriveGeneric #-}

module GameStatus.Talking
    ( TalkingHandler
    , talkingHandler
    , getTalkingPartner
    , getMessage
    , finishTalking
    ) where

import           Actor                (Actor)
import           Data.Binary          (Binary)
import           GHC.Generics         (Generic)
import           GameStatus.Exploring (ExploringHandler)
import           Localization         (MultilingualText)

data TalkingHandler =
    TalkingHandler
        { talkingPartner :: Actor
        , message        :: MultilingualText
        , afterTalking   :: ExploringHandler
        }
    deriving (Show, Ord, Eq, Generic)

instance Binary TalkingHandler

talkingHandler ::
       Actor -> MultilingualText -> ExploringHandler -> TalkingHandler
talkingHandler = TalkingHandler

getTalkingPartner :: TalkingHandler -> Actor
getTalkingPartner (TalkingHandler a _ _) = a

getMessage :: TalkingHandler -> MultilingualText
getMessage (TalkingHandler _ m _) = m

finishTalking :: TalkingHandler -> ExploringHandler
finishTalking TalkingHandler {afterTalking = at} = at
