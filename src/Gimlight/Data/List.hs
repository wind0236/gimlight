module Gimlight.Data.List
    ( intercalateIncludingHeadTail
    ) where

import           Data.List (intercalate)

intercalateIncludingHeadTail :: [a] -> [[a]] -> [a]
intercalateIncludingHeadTail x xs = x ++ intercalate x xs ++ x
