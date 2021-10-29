{-# LANGUAGE OverloadedStrings #-}

module Log
    ( MessageLog
    , emptyLog
    , Message
    , message
    , MessageWriter
    , addMessage
    , addMaybeMessage
    , addMessages
    ) where

import           Control.Monad.Trans.Writer (Writer)
import           Localization               (MultilingualText)

type Message = MultilingualText

type MessageLog = [Message]

type MessageWriter a = Writer MessageLog a

emptyLog :: MessageLog
emptyLog = []

addMessages :: [Message] -> MessageLog -> MessageLog
addMessages xs l = foldl (flip addMessage) l xs

addMessage :: Message -> MessageLog -> MessageLog
addMessage m l = take maxLog (m:l)

addMaybeMessage :: Maybe Message -> MessageLog -> MessageLog
addMaybeMessage (Just m) l = addMessage m l
addMaybeMessage Nothing l  = l

message :: MultilingualText -> Message
message text = text

maxLog :: Int
maxLog = 100
