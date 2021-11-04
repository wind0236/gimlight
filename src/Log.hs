{-# LANGUAGE OverloadedStrings #-}

module Log
    ( MessageLog
    , emptyLog
    , Message
    , message
    , addMessage
    , addMessages
    ) where

import           Localization (MultilingualText)

type Message = MultilingualText

type MessageLog = [Message]

emptyLog :: MessageLog
emptyLog = []

addMessages :: [Message] -> MessageLog -> MessageLog
addMessages xs l = foldl (flip addMessage) l xs

addMessage :: Message -> MessageLog -> MessageLog
addMessage m l = take maxLog (m : l)

message :: MultilingualText -> Message
message text = text

maxLog :: Int
maxLog = 100
