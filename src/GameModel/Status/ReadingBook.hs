{-# LANGUAGE DeriveGeneric #-}

module GameModel.Status.ReadingBook
    ( ReadingBookHandler
    , readingBookHandler
    , getContent
    , finishReading
    ) where

import           Data.Binary                (Binary)
import           GHC.Generics               (Generic)
import           GameModel.Status.Exploring (ExploringHandler, completeThisTurn)
import           Localization               (MultilingualText)

data ReadingBookHandler = ReadingBookHandler
                        { content      :: MultilingualText
                        , afterReading :: ExploringHandler
                        } deriving (Show, Ord, Eq, Generic)

instance Binary ReadingBookHandler

readingBookHandler :: MultilingualText -> ExploringHandler -> ReadingBookHandler
readingBookHandler = ReadingBookHandler

getContent :: ReadingBookHandler -> MultilingualText
getContent (ReadingBookHandler c _) = c

finishReading :: ReadingBookHandler -> Maybe ExploringHandler
finishReading (ReadingBookHandler _ h) = completeThisTurn h
