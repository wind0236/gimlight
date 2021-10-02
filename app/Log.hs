{-# LANGUAGE TupleSections #-}

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

import           Brick.AttrMap   (AttrName)
import           Data.List       (intercalate)
import           Data.List.Split (splitOn)
import           Graphics.Vty    (Color)
import qualified Graphics.Vty    as V
import           UI.Attrs        (emptyAttr, greenAttr, redAttr, whiteAttr)

newtype Message = Message
             { text :: String
             } deriving (Show)

type MessageLog = [Message]

width, height :: Int
width = 80
height = 5

emptyLog :: MessageLog
emptyLog = take height $ replicate height emptyMessage

emptyMessage :: Message
emptyMessage = Message { text = replicate width ' ' }

addMessages :: [Message] -> MessageLog -> MessageLog
addMessages xs l = foldl (flip addMessage) l xs

addMessage :: Message -> MessageLog -> MessageLog
addMessage m l = m:l

addMaybeMessage :: Maybe Message -> MessageLog -> MessageLog
addMaybeMessage (Just m) log = addMessage m log
addMaybeMessage Nothing log  = log

message :: String -> Message
message text = Message { text = text
                       }

messageToAttrNameAndStringList :: Message -> [(AttrName, String)]
messageToAttrNameAndStringList m =  take height $ map (whiteAttr,) $ messageToStringList m

messageToStringList :: Message -> [String]
messageToStringList Message{ text = text } = map (\x -> x ++ replicate (width - length x) ' ') $ concatMap wrapString $ splitStringOnNewLine text

wrapString :: String -> [String]
wrapString = wrapStringAcc []

wrapStringAcc :: [String] -> String -> [String]
wrapStringAcc s "" = s
wrapStringAcc list str = wrapStringAcc newList newStr
    where (newLine, newStr) = splitAt width str
          newList = list ++ [newLine]

splitStringOnNewLine :: String -> [String]
splitStringOnNewLine = splitOn "\n"
