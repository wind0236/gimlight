module Gimlight.Data.String
    ( adjustLength
    , makeTable
    ) where

import           Gimlight.Data.List (intercalateIncludingHeadTail)

adjustLength :: Int -> String -> String
adjustLength n s = s ++ replicate (n - length s) ' '

makeTable :: [[String]] -> String
makeTable [] = "+"
makeTable rows = insertSeps canonicalized
  where
    canonicalized = fmap (fmap (adjustLength cellWidth) . addEmptyElements) rows
    addEmptyElements xs = xs ++ replicate (numCols - length xs) []
    insertSeps = insertHseps . fmap insertVseps
    insertHseps = init . intercalateIncludingHeadTail (hsep ++ "\n")
    insertVseps = (++ "\n") . intercalateIncludingHeadTail "|"
    hsep = intercalateIncludingHeadTail "+" $ replicate numCols cellHsep
    cellHsep = replicate cellWidth '-'
    numCols = maximum $ fmap length rows
    cellWidth = maximum $ fmap (foldl max 0 . fmap length) rows -- Do not replicate `foldl max 0` with `maximum` as the latter will panic if the list is empty.
