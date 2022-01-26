module Gimlight.Action.WaitSpec
    ( spec
    ) where

import           Control.Monad.Writer   (writer)
import           Gimlight.Action        (ActionResult (ActionResult, killed, newCellMap, status),
                                         ActionStatus (Ok))
import           Gimlight.Action.Wait   (waitAction)
import           Gimlight.SetUp.CellMap (initCellMap, initTileCollection,
                                         playerPosition)
import           Test.Hspec             (Spec, describe, it, shouldBe)

spec :: Spec
spec =
    describe "WaitAction" $
    it "returns a Ok result." $ result `shouldBe` expected
  where
    result = waitAction playerPosition initTileCollection initCellMap
    expected = writer (expectedResult, expectedLog)
    expectedResult =
        ActionResult {status = Ok, newCellMap = initCellMap, killed = []}
    expectedLog = []
