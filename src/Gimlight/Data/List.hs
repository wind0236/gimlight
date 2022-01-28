module Gimlight.Data.List
    ( intercalateIncludingHeadTail
    , makeTable
    ) where

import           Data.List (intercalate)

intercalateIncludingHeadTail :: [a] -> [[a]] -> [a]
intercalateIncludingHeadTail x xs = x ++ intercalate x xs ++ x

makeTable :: [[String]] -> String
makeTable [] = "+"
makeTable rows = insertSeps canonicalized
  where
    canonicalized = fmap (fmap addPad . addEmptyElements) rows
    addEmptyElements xs = xs ++ replicate (numCols - length xs) []
    addPad s = s ++ replicate (cellWidth - length s) ' '
    insertSeps = insertHseps . fmap insertVseps
    insertHseps = init . intercalateIncludingHeadTail (hsep ++ "\n")
    insertVseps = (++ "\n") . intercalateIncludingHeadTail "|"
    hsep = intercalateIncludingHeadTail "+" $ replicate numCols cellHsep
    cellHsep = replicate cellWidth '-'
    numCols = maximum $ fmap length rows
    cellWidth = maximum $ fmap (foldl max 0 . fmap length) rows -- Do not replicate `foldl max 0` with `maximum` as the latter will panic if the list is empty.
