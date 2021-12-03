module Main
    ( main
    ) where

import qualified Dungeon.Map.CellSpec
import qualified DungeonSpec
import           Test.Hspec           (Spec, describe, hspec)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Dungeon" DungeonSpec.spec
    describe "Dungeon.Map.Cell" Dungeon.Map.CellSpec.spec
