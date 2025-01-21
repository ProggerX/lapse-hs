import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Lib (Value (..), list, toInfix)

showTests :: [(Value, String)]
showTests =
  [ (list $ map Number [1, 2, 3], "(1 2 3)")
  , (Pair (Number 1) (Pair (Number 2) (Number 3)), "(1 2 . 3)")
  , (Pair (Pair (Number 1) (Number 2)) (Number 3), "((1 . 2) . 3)")
  , (Pair (Pair (Number 1) (Number 2)) (Pair (Number 3) Nil), "((1 . 2) 3)")
  , (Number 5, "5")
  , (Pair (Number 5) Nil, "(5)")
  ]

infixTests :: [(Value, String)]
infixTests =
  [ (list [Name "+", Number 1, Number 2], "1 + 2")
  , (list [Name "+", Number 1, Number 2, Number 3], "1 + 2 + 3")
  , (list [Name "+", Number 1, list [Name "*", Number 2, Number 3]], "1 + 2 * 3")
  , (list [Name "*", Number 1, list [Name "+", Number 2, Number 3]], "1 * (2 + 3)")
  , (list [Name "+", Number 1, list [Name "*", Number 2, Number 3, Number 4]], "1 + 2 * 3 * 4")
  , (list [Name "*", Number 1, list [Name "+", Number 2, Number 3, Number 4]], "1 * (2 + 3 + 4)")
  ]

main :: IO ()
main =
  defaultMain $
    testGroup
      "all"
      [ testGroup
          "show"
          $ map (\(t, x) -> testCase "test" $ show t @?= x) showTests
      , testGroup
          "infix"
          $ map (\(t, x) -> testCase "test" $ toInfix t @?= x) infixTests
      ]
