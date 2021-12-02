module Main
    ( main
    ) where

import qualified Dungeon.Map.CellSpec
import           Test.Hspec           (Spec, describe, hspec)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Dungeon.Map.Cell" Dungeon.Map.CellSpec.spec
