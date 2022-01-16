module Dungeon.Generate.ConfigSpec
    ( spec
    ) where

import           Control.Exception       (evaluate)
import           Dungeon.Generate.Config (config, mapHeightIsTooSmall,
                                          mapWidthIsTooSmall,
                                          maxRoomMustBePositive,
                                          numOfFloorsMustBePositive,
                                          roomMinIsLargerThanRoomMax,
                                          roomMinSizeMustBePositive)
import           Linear.V2               (V2 (V2))
import           Test.Hspec              (Spec, describe, errorCall, it,
                                          shouldThrow)

spec :: Spec
spec =
    describe "config" $ do
        testPanicIfNumOfFloorsIsNotPositive
        testPanicIfMaxRoomsIsNotPositive
        testPanicIfRoomMinSizeIsNotPositive
        testPanicIfWidthIsTooSmall
        testPanicIfHeightIsTooSmall
        testPanicIfRoomMinSizeIsLargerThanRoomMaxSize

testPanicIfNumOfFloorsIsNotPositive :: Spec
testPanicIfNumOfFloorsIsNotPositive =
    it "panics if the given number of floors is not positive." $
    evaluate (config 0 1 1 1 (V2 100 100)) `shouldThrow`
    errorCall numOfFloorsMustBePositive

testPanicIfMaxRoomsIsNotPositive :: Spec
testPanicIfMaxRoomsIsNotPositive =
    it "panics if the given number of maximum rooms is not positive." $
    evaluate (config 1 0 1 1 (V2 100 100)) `shouldThrow`
    errorCall maxRoomMustBePositive

testPanicIfRoomMinSizeIsNotPositive :: Spec
testPanicIfRoomMinSizeIsNotPositive =
    it "panics if the given number of minimum room size is not positive." $
    evaluate (config 1 1 0 1 (V2 100 100)) `shouldThrow`
    errorCall roomMinSizeMustBePositive

testPanicIfWidthIsTooSmall :: Spec
testPanicIfWidthIsTooSmall =
    it "panics if the given map width is too small" $
    evaluate (config 1 1 1 1 (V2 width 1)) `shouldThrow`
    errorCall (mapWidthIsTooSmall width)
  where
    width = 1

testPanicIfHeightIsTooSmall :: Spec
testPanicIfHeightIsTooSmall =
    it "panics if the given map height is too small" $
    evaluate (config 1 1 1 1 (V2 100 height)) `shouldThrow`
    errorCall (mapHeightIsTooSmall height)
  where
    height = 1

testPanicIfRoomMinSizeIsLargerThanRoomMaxSize :: Spec
testPanicIfRoomMinSizeIsLargerThanRoomMaxSize =
    it "panics if the given room minimum size is larger than the room maximum size" $
    evaluate (config 1 1 rmin rmax (V2 100 100)) `shouldThrow`
    errorCall (roomMinIsLargerThanRoomMax rmin rmax)
  where
    rmin = 2
    rmax = 1
