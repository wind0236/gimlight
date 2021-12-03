module Main
    ( main
    ) where

import qualified Action.DropSpec
import qualified Dungeon.Map.CellSpec
import qualified DungeonSpec
import           Test.Hspec           (Spec, describe, hspec)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Action.DropSpec" Action.DropSpec.spec
    describe "Dungeon" DungeonSpec.spec
    describe "Dungeon.Map.Cell" Dungeon.Map.CellSpec.spec
