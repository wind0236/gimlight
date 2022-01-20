module Generator
    ( generateNonPositive
    , generatePositiveBigSmallNumbers
    ) where

import           Test.QuickCheck (Arbitrary (arbitrary), Gen, suchThat)

generateNonPositive :: Gen Int
generateNonPositive = negate . abs <$> arbitrary

generatePositiveBigSmallNumbers :: Gen (Int, Int)
generatePositiveBigSmallNumbers = arbitrary `suchThat` \(a, b) -> a > b && b > 0
