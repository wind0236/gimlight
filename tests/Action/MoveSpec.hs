module Action.MoveSpec
    ( spec
    ) where

import           Action               (ActionResult (ActionResult, killed, newCellMap, status),
                                       ActionResultWithLog,
                                       ActionStatus (Failed, Ok))
import           Action.Move          (moveAction)
import           Control.Monad.Writer (writer)
import           Data.Maybe           (fromJust)
import           Dungeon.Map.Cell     (locateActorAt, removeActorAt)
import           Linear.V2            (V2 (V2))
import qualified Localization.Texts   as T
import           SetUp                (initCellMap, initTileCollection,
                                       playerPosition)
import           Test.Hspec           (Spec, it, shouldBe)

spec :: Spec
spec = do
    testMoveSucceed
    testTriedToMoveToUnwalkablePlace
    testTriedToMoveWhereActorExists

testMoveSucceed :: Spec
testMoveSucceed =
    it "succeeds to move if no actor is on the destination and the destination is walkable" $
    resultWhenMoveOffsetTo moveTo `shouldBe` succeed moveTo
  where
    moveTo = V2 1 1

testTriedToMoveToUnwalkablePlace :: Spec
testTriedToMoveToUnwalkablePlace =
    it "fails to move because the destination is not walkable." $
    resultWhenMoveOffsetTo (V2 0 1) `shouldBe` failed

testTriedToMoveWhereActorExists :: Spec
testTriedToMoveWhereActorExists =
    it "fails to move because there is an actor on the destination." $
    resultWhenMoveOffsetTo (V2 1 0) `shouldBe` failed

succeed :: V2 Int -> ActionResultWithLog
succeed offset = writer (result, [])
  where
    result =
        ActionResult {status = Ok, newCellMap = cellMapWithPlayer, killed = []}
    cellMapWithPlayer =
        fromJust $
        removeActorAt playerPosition initCellMap >>=
        (\(p, ncm) ->
             locateActorAt initTileCollection p (playerPosition + offset) ncm)

failed :: ActionResultWithLog
failed = writer (result, l)
  where
    result =
        ActionResult {status = Failed, newCellMap = initCellMap, killed = []}
    l = [T.youCannotMoveThere]

resultWhenMoveOffsetTo :: V2 Int -> ActionResultWithLog
resultWhenMoveOffsetTo offset =
    moveAction offset playerPosition initTileCollection initCellMap
