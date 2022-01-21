module Dungeon.GenerateSpec
    ( spec
    ) where

import           Control.Lens                (_1, (^.))
import           Control.Monad.State         (State, StateT, evalState,
                                              evalStateT)
import           Coord                       (Coord)
import           Data.Map                    (empty)
import           Data.Tree                   (Tree)
import           Dungeon                     (Dungeon)
import qualified Dungeon                     as D
import           Dungeon.Generate            (generateMultipleFloorsDungeon)
import           Dungeon.Generate.Config     (Config, config, getMapSize)
import           Dungeon.Identifier          (Identifier (Beaeve))
import           Dungeon.Map.Cell            (CellMap, widthAndHeight)
import           Dungeon.Map.Tile            (TileCollection)
import           Dungeon.Map.Tile.JSONReader (addTileFile)
import           IndexGenerator              (IndexGenerator, generator)
import           Linear.V2                   (V2 (V2))
import           System.Random               (StdGen, mkStdGen)
import           Test.Hspec                  (Spec, describe, it, runIO,
                                              shouldBe)
import           Test.Hspec.QuickCheck       (modifyMaxSuccess)
import           Test.QuickCheck             (Arbitrary (arbitrary), Gen,
                                              forAll, suchThat)

spec :: Spec
spec = testSizeIsCorrect

testSizeIsCorrect :: Spec
testSizeIsCorrect = do
    tc <-
        runIO $
        addTileFile "tiles/tiles.json" empty >>= addTileFile "tiles/stairs.json"
    modifyMaxSuccess (const 1) $
        describe "generateMultipleFloorsDungeon" $
        it "generates a dungeon with the specified map size" $
        forAll ((,) <$> generateConfig <*> arbitrary) $ propertyFunc tc
  where
    propertyFunc tc (cfg, g) =
        mapM_ (`shouldBe` getMapSize cfg) $ dungeonSize tc cfg g
    dungeonSize tc cfg = fmap widthAndHeight . generateMap tc cfg

generateMap :: TileCollection -> Config -> Int -> Tree CellMap
generateMap tc cfg g = fmap (^. D.cellMap) tree
  where
    tree =
        extractDungeonTree (mkStdGen g) $
        generateMultipleFloorsDungeon tc cfg Beaeve

extractDungeonTree ::
       StdGen
    -> StateT IndexGenerator (State StdGen) (Tree Dungeon, Coord)
    -> Tree Dungeon
extractDungeonTree g = (^. _1) . flip evalState g . flip evalStateT generator

generateConfig :: Gen Config
generateConfig = fmap tupleToConfig $ arbitrary `suchThat` validValue
  where
    validValue (numOfFloors, maxRooms, roomMinSize, roomMaxSize, width, height) =
        numOfFloors > 0 &&
        maxRooms > 0 &&
        roomMinSize > 0 &&
        roomMaxSize > roomMinSize && width > roomMaxSize && height > roomMaxSize
    tupleToConfig (nf, mr, rmin, rmax, width, height) =
        config nf mr rmin rmax (V2 width height)
