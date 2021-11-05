{-# LANGUAGE DeriveGeneric #-}

module GameModel.Status.SelectingItemToDrop
    ( SelectingItemToDropHandler
    , selectingItemToDropHandler
    , getItems
    , getSelectingIndex
    , selectPrevItem
    , selectNextItem
    , finishSelecting
    ) where

import           Data.Binary                (Binary)
import           Dungeon.Item               (Item)
import           GHC.Generics               (Generic)
import           GameModel.Status.Exploring (ExploringHandler)

data SelectingItemToDropHandler =
    SelectingItemToDropHandler
        { items          :: [Item]
        , selecting      :: Int
        , afterSelecting :: ExploringHandler
        }
    deriving (Show, Ord, Eq, Generic)

instance Binary SelectingItemToDropHandler

selectingItemToDropHandler ::
       [Item] -> ExploringHandler -> SelectingItemToDropHandler
selectingItemToDropHandler is = SelectingItemToDropHandler is 0

getItems :: SelectingItemToDropHandler -> [Item]
getItems SelectingItemToDropHandler {items = is} = is

getSelectingIndex :: SelectingItemToDropHandler -> Maybe Int
getSelectingIndex SelectingItemToDropHandler {items = is, selecting = n} =
    if null is
        then Nothing
        else Just n

selectPrevItem :: SelectingItemToDropHandler -> SelectingItemToDropHandler
selectPrevItem sh@SelectingItemToDropHandler {items = is, selecting = n}
    | null is = sh
    | otherwise = sh {selecting = (n - 1) `mod` length is}

selectNextItem :: SelectingItemToDropHandler -> SelectingItemToDropHandler
selectNextItem sh@SelectingItemToDropHandler {items = is, selecting = n}
    | null is = sh
    | otherwise = sh {selecting = (n + 1) `mod` length is}

finishSelecting :: SelectingItemToDropHandler -> ExploringHandler
finishSelecting SelectingItemToDropHandler {afterSelecting = a} = a
