import System.Exit
import Test.HUnit
import qualified Data.Classifier as C
import qualified Data.Map as M

test1 = TestCase (assertEqual "for C.empty" (C.Classifier M.empty M.empty) C.empty)

tests = TestList [
         TestLabel "test1" test1
        ]

main :: IO()
main = do
  counts <- runTestTT tests
  case failures counts of
    0 -> exitSuccess
    _ -> exitFailure
