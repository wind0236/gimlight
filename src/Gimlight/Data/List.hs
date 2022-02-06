module Gimlight.Data.List
    ( intercalateIncludingHeadTail
    , filterAll
    ) where

import           Data.List (intercalate)

intercalateIncludingHeadTail :: [a] -> [[a]] -> [a]
intercalateIncludingHeadTail x xs = x ++ intercalate x xs ++ x

filterAll :: [a -> Bool] -> [a] -> [a]
filterAll preds xs = foldr filter xs preds
