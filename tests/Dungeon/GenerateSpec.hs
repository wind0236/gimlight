module Dungeon.GenerateSpec
    ( spec
    ) where

import           Control.Lens                (_1, (^.))
import           Control.Monad.State         (evalState, evalStateT)
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
import           Test.Hspec                  (Spec, describe, it, runIO)
import           Test.QuickCheck             (Testable (property), chooseInt)
import           UI.Draw.Config              (tileColumns, tileRows)

spec :: Spec
spec = testSizeIsCorrect

testSizeIsCorrect :: Spec
testSizeIsCorrect = do
    tc <- runIO $ addTileFile "tiles/tiles.json" empty
    describe "generateMultipleFloorsDungeon" $
        it "generates a dungeon with the specified map size" $
        property $ propertyFunc tc
  where
    propertyFunc tc = do
        width <- chooseInt (tileColumns, 100)
        height <- chooseInt (tileRows, 100)
        return $ dungeonSize tc (V2 width height) == V2 width height
    dungeonSize tc sz =
        let Node d _ =
                evalState
                    (evalStateT
                         (generateMultipleFloorsDungeon tc (cfg sz) Beaeve)
                         generator)
                    (mkStdGen 0) ^.
                _1
         in widthAndHeight $ d ^. D.cellMap
    cfg = config 1 3 2 3
