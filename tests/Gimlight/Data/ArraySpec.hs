module Gimlight.Data.ArraySpec
    ( spec
    ) where

import           Control.Lens        (Ixed (ix), (^?))
import           Data.Array          (array)
import           Data.Maybe          (isNothing)
import           Gimlight.Data.Array (toRowsList)
import           Linear.V2           (V2 (V2))
import           Test.Hspec          (Spec, it, shouldBe)

spec :: Spec
spec = do
    testArrayAccessingOutOfBounds
    testToRowsList

testArrayAccessingOutOfBounds :: Spec
testArrayAccessingOutOfBounds =
    it "returns a `Nothing` if we try to access to an index which does not exist with `^?`" $
    isNothing $ arr ^? ix (u + 1)
  where
    arr = array (0, u) [(x, 0 :: Int) | x <- [0 :: Int .. u]]
    u = 3

testToRowsList :: Spec
testToRowsList =
    it "converts an array to two dimensional array." $
    result `shouldBe` expected
  where
    result =
        toRowsList $
        array
            bounds
            [(V2 0 0, 'a'), (V2 1 0, 'b'), (V2 0 1, 'c'), (V2 1 1, 'd')]
    bounds :: (V2 Int, V2 Int)
    bounds = (V2 0 0, V2 1 1)
    expected = [['a', 'b'], ['c', 'd']]
