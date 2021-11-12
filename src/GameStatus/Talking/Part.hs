{-# LANGUAGE DeriveGeneric #-}

module GameStatus.Talking.Part
    ( TalkingPart(..)
    , SelectionHandler
    , selectionHandler
    , getQuestion
    , getChoices
    , getSelectingIndex
    , proceedTalking
    , selectPrevChoice
    , selectNextChoice
    ) where

import           Data.Binary        (Binary)
import           Data.List.NonEmpty (NonEmpty, toList)
import qualified Data.List.NonEmpty as NonEmpty
import           GHC.Generics       (Generic)
import           Localization       (MultilingualText)

data SelectionHandler =
    SelectionHandler
        { question        :: MultilingualText
        , choicesAndNexts :: NonEmpty (MultilingualText, Maybe TalkingPart)
        , selectingIndex  :: Int
        }
    deriving (Show, Ord, Eq, Generic)

instance Binary SelectionHandler

newtype TalkingPart =
    Selection SelectionHandler
    deriving (Show, Ord, Eq, Generic)

instance Binary TalkingPart

selectionHandler ::
       MultilingualText
    -> NonEmpty (MultilingualText, Maybe TalkingPart)
    -> SelectionHandler
selectionHandler q c = SelectionHandler q c 0

getQuestion :: SelectionHandler -> MultilingualText
getQuestion = question

getChoices :: SelectionHandler -> [MultilingualText]
getChoices = toList . NonEmpty.map fst . choicesAndNexts

getSelectingIndex :: SelectionHandler -> Maybe Int
getSelectingIndex (SelectionHandler _ cs n) =
    if null cs
        then Nothing
        else Just n

proceedTalking :: TalkingPart -> Maybe TalkingPart
proceedTalking (Selection h) = select h

selectPrevChoice :: TalkingPart -> TalkingPart
selectPrevChoice (Selection (SelectionHandler q cns idx)) =
    Selection $ SelectionHandler q cns newIdx
  where
    newIdx = (idx - 1) `mod` length cns

selectNextChoice :: TalkingPart -> TalkingPart
selectNextChoice (Selection (SelectionHandler q cns idx)) =
    Selection $ SelectionHandler q cns newIdx
  where
    newIdx = (idx + 1) `mod` length cns

select :: SelectionHandler -> Maybe TalkingPart
select (SelectionHandler _ xs n) = snd $ xs NonEmpty.!! n
