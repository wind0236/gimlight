module Dungeon.GenerateSpec
    ( spec
    ) where

import           Control.Lens                (_1, (^.))
import           Data.Map                    (empty)
import           Data.Tree                   (Tree (Node))
import qualified Dungeon                     as D
import           Dungeon.Generate            (generateMultipleFloorsDungeon)
import           Dungeon.Generate.Config     (config)
import           Dungeon.Identifier          (Identifier (Beaeve))
import           Dungeon.Map.Cell            (widthAndHeight)
import           Dungeon.Map.Tile.JSONReader (addTileFile)
import           IndexGenerator              (generator)
import           Linear.V2                   (V2 (V2))
import           System.Random               (mkStdGen)
import           Test.Hspec                  (Spec, describe, it, runIO,
                                              shouldBe)

spec :: Spec
spec = testSizeIsCorrect

testSizeIsCorrect :: Spec
testSizeIsCorrect = do
    tc <- runIO $ addTileFile "tiles/tiles.json" empty
    describe "generateMultipleFloorsDungeon" $
        it "generates a dungeon with the specified map size" $
        result tc `shouldBe` sz
  where
    result tc =
        let Node d _ =
                generateMultipleFloorsDungeon
                    (mkStdGen 0)
                    generator
                    tc
                    cfg
                    Beaeve ^.
                _1
         in widthAndHeight $ d ^. D.cellMap
    cfg = config 1 3 2 3 sz
    sz = V2 100 100
