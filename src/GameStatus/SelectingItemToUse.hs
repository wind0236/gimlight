{-# LANGUAGE DeriveGeneric #-}

module GameStatus.SelectingItemToUse
    ( SelectingItemToUseHandler
    , selectingItemToUseHandler
    , getItems
    , getSelectingIndex
    , selectPrevItem
    , selectNextItem
    , finishSelecting
    ) where

import           Data.Binary          (Binary)
import           GHC.Generics         (Generic)
import           GameStatus.Exploring (ExploringHandler)
import           Item                 (Item)

data SelectingItemToUseHandler =
    SelectingItemToUseHandler
        { items          :: [Item]
        , selecting      :: Int
        , afterSelecting :: ExploringHandler
        }
    deriving (Show, Ord, Eq, Generic)

instance Binary SelectingItemToUseHandler

selectingItemToUseHandler ::
       [Item] -> ExploringHandler -> SelectingItemToUseHandler
selectingItemToUseHandler is = SelectingItemToUseHandler is 0

getItems :: SelectingItemToUseHandler -> [Item]
getItems SelectingItemToUseHandler {items = is} = is

getSelectingIndex :: SelectingItemToUseHandler -> Maybe Int
getSelectingIndex SelectingItemToUseHandler {items = is, selecting = n} =
    if null is
        then Nothing
        else Just n

selectPrevItem :: SelectingItemToUseHandler -> SelectingItemToUseHandler
selectPrevItem sh@SelectingItemToUseHandler {items = is, selecting = n}
    | null is = sh
    | otherwise = sh {selecting = (n - 1) `mod` length is}

selectNextItem :: SelectingItemToUseHandler -> SelectingItemToUseHandler
selectNextItem sh@SelectingItemToUseHandler {items = is, selecting = n}
    | null is = sh
    | otherwise = sh {selecting = (n + 1) `mod` length is}

finishSelecting :: SelectingItemToUseHandler -> ExploringHandler
finishSelecting SelectingItemToUseHandler {afterSelecting = a} = a
