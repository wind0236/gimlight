{-# LANGUAGE OverloadedStrings #-}

module Action.MeleeSpec
    ( spec
    ) where

import           Action               (ActionResult (ActionResult, killed, newDungeon, status),
                                       ActionStatus (Ok))
import           Action.Melee         (meleeAction)
import           Actor                (Actor, attackFromTo, monster)
import           Actor.Identifier     (Identifier (Orc))
import qualified Actor.Status         as S
import           Actor.Status.Hp      (hp)
import           Control.Lens         ((&), (.~), (^.))
import           Control.Monad.Writer (runWriter, writer)
import           Data.Array           (array)
import           Data.Maybe           (fromJust)
import           Dungeon              (Dungeon, dungeon, pushActor)
import qualified Dungeon              as D
import           Dungeon.Identifier   (Identifier (Beaeve))
import           Dungeon.Map.Cell     (TileIdLayer (TileIdLayer), cellMap,
                                       removeActorAt)
import           Dungeon.Map.Tile     (TileCollection, tile)
import           IndexGenerator       (IndexGenerator, generator)
import           Linear.V2            (V2 (V2))
import           Test.Hspec           (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
    testKill
    testDamage

testKill :: Spec
testKill =
    describe "Strongest orc" $ do
        it "kills the weakest orc" $ result `shouldBe` expected
        it "returns a Nothing defender" $ newDefender `shouldBe` Nothing
  where
    result = meleeAction (V2 1 1) (V2 0 0) s initTileCollection d
    expected = writer (expectedResult, expectedLog)
    expectedResult =
        ActionResult
            { status = Ok
            , newDungeon = pushActor (V2 0 0) newAttacker dungeonWithoutDefender
            , killed = [defender]
            }
    ((newAttacker, newDefender), expectedLog) =
        runWriter $ attackFromTo s defender
    (defender, dungeonWithoutDefender) =
        case removeActorAt (V2 1 1) (d ^. D.cellMap) of
            Just (a, newCellMap) -> (a, d & D.cellMap .~ newCellMap)
            Nothing              -> error "unreachable."
    s = fst $ strongest g
    (d, g) = initDungeon

testDamage :: Spec
testDamage =
    describe "Strongest orc" $
    it "attacks to the intermediate orc" $ result `shouldBe` expected
  where
    result = meleeAction (V2 0 1) (V2 0 0) s initTileCollection d
    expected = writer (expectedResult, expectedLog)
    expectedResult =
        ActionResult
            { status = Ok
            , newDungeon =
                  pushActor (V2 0 0) newAttacker $
                  pushActor
                      (V2 0 1)
                      (fromJust newDefender)
                      dungeonWithoutDefender
            , killed = []
            }
    ((newAttacker, newDefender), expectedLog) =
        runWriter $ attackFromTo s defender
    (defender, dungeonWithoutDefender) =
        case removeActorAt (V2 0 1) (d ^. D.cellMap) of
            Just (a, newCellMap) -> (a, d & D.cellMap .~ newCellMap)
            Nothing              -> error "unreachable"
    s = fst $ strongest g
    (d, g) = initDungeon

initDungeon :: (Dungeon, IndexGenerator)
initDungeon =
    ( pushActor (V2 1 1) w $
      pushActor (V2 0 1) i $ pushActor (V2 1 0) s $ dungeon cm Beaeve
    , g''')
  where
    cm =
        cellMap $
        array (V2 0 0, V2 1 1) [(V2 x y, walkable) | x <- [0, 1], y <- [0, 1]]
    walkable = TileIdLayer (Just 0) (Just 0)
    (w, g') = weakest generator
    (i, g'') = intermediate g'
    (s, g''') = strongest g''

initTileCollection :: TileCollection
initTileCollection = array (0, 0) [(0, tile True True)]

strongest :: IndexGenerator -> (Actor, IndexGenerator)
strongest g = monster g Orc (S.status (hp 100) 100 100) ""

intermediate :: IndexGenerator -> (Actor, IndexGenerator)
intermediate g = monster g Orc (S.status (hp 100) 50 50) ""

weakest :: IndexGenerator -> (Actor, IndexGenerator)
weakest g = monster g Orc (S.status (hp 1) 0 0) ""
