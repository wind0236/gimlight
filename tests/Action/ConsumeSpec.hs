module Action.ConsumeSpec
    ( spec
    ) where

import           Action               (ActionResult (ActionResult, killed, newCellMap, status),
                                       ActionStatus (Ok, ReadingStarted))
import           Action.Consume       (consumeAction)
import           Actor                (getIdentifier, inventoryItems)
import           Actor.Identifier     (toName)
import           Control.Lens         ((%~), (&))
import           Control.Monad.Writer (writer)
import           Data.Maybe           (fromJust)
import           Dungeon.Map.Cell     (locateActorAt, removeActorAt)
import           Inventory            (removeNthItem)
import           Item                 (Effect (Book, Heal), getEffect, herb,
                                       sampleBook)
import           Item.Heal            (getHealAmount)
import qualified Localization.Texts   as T
import           SetUp                (initCellMap, initTileCollection,
                                       orcWithHerbPosition, playerPosition)
import           Test.Hspec           (Spec, it, shouldBe)

spec :: Spec
spec = do
    testStartReadingBook
    testConsumeHerb

testStartReadingBook :: Spec
testStartReadingBook =
    it "returns a ReadingStarted result if an actor uses a book" $
    result `shouldBe` expected
  where
    result = consumeAction 0 playerPosition initTileCollection initCellMap
    expected = writer (expectedResult, expectedLog)
    expectedResult =
        ActionResult
            { status = ReadingStarted $ bookContent $ getEffect sampleBook
            , newCellMap = initCellMap
            , killed = []
            }
    expectedLog = []
    bookContent (Book c) = c
    bookContent _        = error "Not a book."

testConsumeHerb :: Spec
testConsumeHerb =
    it "returns a Ok result if an actor uses a herb" $
    result `shouldBe` expected
  where
    result = consumeAction 0 orcWithHerbPosition initTileCollection initCellMap
    expected = writer (expectedResult, expectedLog)
    expectedResult =
        ActionResult
            {status = Ok, newCellMap = cellMapAfterConsuming, killed = []}
    expectedLog =
        [ T.healed (toName $ getIdentifier orcWithItem) $
          healAmount $ getEffect herb
        ]
    cellMapAfterConsuming =
        fromJust $
        removeActorAt orcWithHerbPosition initCellMap >>=
        (\(a, cm) ->
             locateActorAt
                 initTileCollection
                 (a & inventoryItems %~ (snd . removeNthItem 0))
                 orcWithHerbPosition
                 cm)
    orcWithItem = fst $ fromJust $ removeActorAt orcWithHerbPosition initCellMap
    healAmount (Heal h) = getHealAmount h
    healAmount _        = error "Not a healer."
