module Gimlight.Data.Either
    ( expectRight
    ) where

import           GHC.Stack (HasCallStack)

expectRight :: (Show b, HasCallStack) => String -> Either b a -> a
expectRight _ (Right x)  = x
expectRight msg (Left x) = error $ msg ++ ": " ++ show x
