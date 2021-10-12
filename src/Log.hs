module Log
    ( MessageLog
    , emptyLog
    , Message
    , message
    , addMessage
    , addMaybeMessage
    , messageToStringList
    , messageToAttrNameAndStringList
    , height
    , width
    , emptyMessage
    , addMessages
    ) where

import           Data.List.Split (splitOn)

type Message = String

type MessageLog = [Message]

width, height :: Int
width = 80
height = 5

emptyLog :: MessageLog
emptyLog = take height $ replicate height emptyMessage

emptyMessage :: Message
emptyMessage = replicate width ' '

addMessages :: [Message] -> MessageLog -> MessageLog
addMessages xs l = foldl (flip addMessage) l xs

addMessage :: Message -> MessageLog -> MessageLog
addMessage m l = m:l

addMaybeMessage :: Maybe Message -> MessageLog -> MessageLog
addMaybeMessage (Just m) l = addMessage m l
addMaybeMessage Nothing l  = l

message :: String -> Message
message text = text

messageToAttrNameAndStringList :: Message -> [String]
messageToAttrNameAndStringList m =  take height $ messageToStringList m

messageToStringList :: Message -> [String]
messageToStringList text = map (\x -> x ++ replicate (width - length x) ' ') $ concatMap wrapString $ splitStringOnNewLine text

wrapString :: String -> [String]
wrapString = wrapStringAcc []

wrapStringAcc :: [String] -> String -> [String]
wrapStringAcc s "" = s
wrapStringAcc list str = wrapStringAcc newList newStr
    where (newLine, newStr) = splitAt width str
          newList = list ++ [newLine]

splitStringOnNewLine :: String -> [String]
splitStringOnNewLine = splitOn "\n"
