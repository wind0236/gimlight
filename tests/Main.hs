module Main
    ( main
    ) where

import qualified Action.DropSpec
import qualified Action.PickUpSpec
import qualified Action.WaitSpec
import qualified Dungeon.Map.CellSpec
import qualified DungeonSpec
import qualified FovSpec
import           Test.Hspec           (Spec, describe, hspec)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Action.Drop" Action.DropSpec.spec
    describe "Action.PickUp" Action.PickUpSpec.spec
    describe "Action.Wait" Action.WaitSpec.spec
    describe "Dungeon" DungeonSpec.spec
    describe "Dungeon.Map.Cell" Dungeon.Map.CellSpec.spec
    describe "Fov" FovSpec.spec
