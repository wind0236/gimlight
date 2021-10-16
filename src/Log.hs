{-# LANGUAGE OverloadedStrings #-}

module Log
    ( MessageLog
    , emptyLog
    , Message
    , message
    , addMessage
    , addMaybeMessage
    , addMessages
    ) where

import           Data.Text (Text)

type Message = Text

type MessageLog = [Message]

emptyLog :: MessageLog
emptyLog = []

addMessages :: [Message] -> MessageLog -> MessageLog
addMessages xs l = foldl (flip addMessage) l xs

addMessage :: Message -> MessageLog -> MessageLog
addMessage m l = take maxLog (m:l)

addMaybeMessage :: Maybe Message -> MessageLog -> MessageLog
addMaybeMessage (Just m) l = addMessage m l
addMaybeMessage Nothing l  = l

message :: Text -> Message
message text = text

maxLog :: Int
maxLog = 100