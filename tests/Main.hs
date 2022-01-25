module Main
    ( main
    ) where

import qualified Action.ConsumeSpec
import qualified Action.DropSpec
import qualified Action.MeleeSpec
import qualified Action.MoveSpec
import qualified Action.PickUpSpec
import qualified Action.WaitSpec
import qualified Data.ArraySpec
import qualified Dungeon.Generate.ConfigSpec
import qualified Dungeon.GenerateSpec
import qualified Dungeon.Map.CellSpec
import qualified Dungeon.Map.JSONReaderSpec
import qualified Dungeon.Map.Tile.JSONReaderSpec
import qualified FovSpec
import           Test.Hspec                      (Spec, describe, hspec)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Action.Consume" Action.ConsumeSpec.spec
    describe "Action.Drop" Action.DropSpec.spec
    describe "Action.Melee" Action.MeleeSpec.spec
    describe "Action.Move" Action.MoveSpec.spec
    describe "Action.PickUp" Action.PickUpSpec.spec
    describe "Action.Wait" Action.WaitSpec.spec
    describe "Data.Array" Data.ArraySpec.spec
    describe "Dungeon.Generate" Dungeon.GenerateSpec.spec
    describe "Dungeon.Generate.Config" Dungeon.Generate.ConfigSpec.spec
    describe "Dungeon.Map.Cell" Dungeon.Map.CellSpec.spec
    describe "Dungeon.Map.JSONReader" Dungeon.Map.JSONReaderSpec.spec
    describe "Dungeon.Map.Tile.JSONReader" Dungeon.Map.Tile.JSONReaderSpec.spec
    describe "Fov" FovSpec.spec
