{-# LANGUAGE OverloadedStrings #-}

module Log
    ( MessageLog
    , emptyLog
    , Message
    , message
    , addMessage
    , addMaybeMessage
    , messageToTextList
    , messageToAttrNameAndTextList
    , height
    , width
    , emptyMessage
    , addMessages
    ) where

import           Data.Text (Text, append, pack, splitOn)
import qualified Data.Text as T

type Message = Text

type MessageLog = [Message]

width, height :: Int
width = 80
height = 5

emptyLog :: MessageLog
emptyLog = take height $ replicate height emptyMessage

emptyMessage :: Message
emptyMessage = pack $ replicate width ' '

addMessages :: [Message] -> MessageLog -> MessageLog
addMessages xs l = foldl (flip addMessage) l xs

addMessage :: Message -> MessageLog -> MessageLog
addMessage m l = m:l

addMaybeMessage :: Maybe Message -> MessageLog -> MessageLog
addMaybeMessage (Just m) l = addMessage m l
addMaybeMessage Nothing l  = l

message :: Text -> Message
message text = text

messageToAttrNameAndTextList :: Message -> [Text]
messageToAttrNameAndTextList m =  take height $ messageToTextList m

messageToTextList :: Message -> [Text]
messageToTextList text = map (\x -> x `append` pack (replicate (width - T.length x) ' ')) $ concatMap wrapText $ splitTextOnNewLine text

wrapText :: Text -> [Text]
wrapText = wrapTextAcc []

wrapTextAcc :: [Text] -> Text -> [Text]
wrapTextAcc s "" = s
wrapTextAcc list str = wrapTextAcc newList newStr
    where (newLine, newStr) = T.splitAt width str
          newList = list ++ [newLine]

splitTextOnNewLine :: Text -> [Text]
splitTextOnNewLine = splitOn "\n"
