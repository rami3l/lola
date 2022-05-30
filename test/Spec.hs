import Relude
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import Test.Tasty.QuickCheck as QC (testProperty)
import Test.Tasty.SmallCheck as SC (testProperty)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [quickCheckTests, unitTests]

smallCheckTests :: TestTree
smallCheckTests =
    testGroup
        "SmallCheck Tests"
        []

-- [ SC.testProperty "String length <= 3" $
--     \s -> length (take 3 (s :: String)) <= 3
-- ]

quickCheckTests :: TestTree
quickCheckTests =
    testGroup
        "QuickCheck Tests"
        []

-- [ QC.testProperty "String length <= 3" $
--     \s -> length (take 3 (s :: String)) <= 3
-- ]

unitTests :: TestTree
unitTests =
    testGroup
        "CHANGE THIS PLACEHOLDER PLEASE!"
        [ testCase "it's not 1984" $
            let expected = 4
             in assertEqual "Can calculate 2 + 2" (2 + 2) expected
        ]