module Gimlight.Data.Maybe
  ( expectJust,
  )
where

import Data.Maybe (fromMaybe)
import GHC.Stack (HasCallStack)

expectJust :: HasCallStack => String -> Maybe a -> a
expectJust s = fromMaybe (error s)
