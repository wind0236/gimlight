module Gimlight.Data.ArraySpec
    ( spec
    ) where

import           Control.Lens (Ixed (ix), (^?))
import           Data.Array   (array)
import           Data.Maybe   (isNothing)
import           Test.Hspec   (Spec, it)

spec :: Spec
spec = testArrayAccessingOutOfBounds

testArrayAccessingOutOfBounds :: Spec
testArrayAccessingOutOfBounds =
    it "returns a `Nothing` if we try to access to an index which does not exist with `^?`" $
    isNothing $ arr ^? ix (u + 1)
  where
    arr = array (0, u) [(x, 0 :: Int) | x <- [0 :: Int .. u]]
    u = 3
