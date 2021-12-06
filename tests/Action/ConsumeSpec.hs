module Action.ConsumeSpec
    ( spec
    ) where

import           Action               (ActionResult (ActionResult, killed, newCellMap, status),
                                       ActionStatus (Ok, ReadingStarted))
import           Action.Consume       (consumeAction)
import           Actor                (getIdentifier, inventoryItems, player)
import           Actor.Identifier     (toName)
import           Actor.Inventory      (addItem)
import           Control.Lens         ((%~), (&))
import           Control.Monad.Writer (writer)
import           Coord                (Coord)
import           Data.Array           (array)
import           Data.Maybe           (fromJust)
import           Dungeon.Map.Cell     (CellMap, TileIdLayer (TileIdLayer),
                                       cellMap, locateActorAt)
import           Dungeon.Map.Tile     (TileCollection, tile)
import           IndexGenerator       (generator)
import           Item                 (Effect (Book, Heal), getEffect, herb,
                                       sampleBook)
import           Item.Heal            (getHealAmount)
import           Linear.V2            (V2 (V2))
import qualified Localization.Texts   as T
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
    result = consumeAction 0 playerPosition p initTileCollection initCellMap
    expected = writer (expectedResult, expectedLog)
    expectedResult =
        ActionResult
            { status = ReadingStarted $ bookContent $ getEffect sampleBook
            , newCellMap = cellMapWithPlayer
            , killed = []
            }
    expectedLog = []
    cellMapWithPlayer = fromJust $ locateActorAt p playerPosition initCellMap
    bookContent (Book c) = c
    bookContent _        = error "Not a book."
    p =
        fst (player generator) &
        inventoryItems %~ (fromJust . addItem sampleBook)

testConsumeHerb :: Spec
testConsumeHerb =
    it "returns a Ok result if an actor uses a herb" $
    result `shouldBe` expected
  where
    result =
        consumeAction
            0
            playerPosition
            playerWithItem
            initTileCollection
            initCellMap
    expected = writer (expectedResult, expectedLog)
    expectedResult =
        ActionResult {status = Ok, newCellMap = cellMapWithPlayer, killed = []}
    expectedLog =
        [ T.healed (toName $ getIdentifier playerWithItem) $
          healAmount $ getEffect herb
        ]
    cellMapWithPlayer =
        fromJust $ locateActorAt playerWithoutItem playerPosition initCellMap
    playerWithItem =
        playerWithoutItem & inventoryItems %~ (fromJust . addItem herb)
    playerWithoutItem = fst $ player generator
    healAmount (Heal h) = getHealAmount h
    healAmount _        = error "Not a healer."

initCellMap :: CellMap
initCellMap =
    cellMap $ array (V2 0 0, V2 0 0) [(V2 0 0, TileIdLayer Nothing Nothing)]

initTileCollection :: TileCollection
initTileCollection = array (0, 0) [(0, tile True True)]

playerPosition :: Coord
playerPosition = V2 0 0
