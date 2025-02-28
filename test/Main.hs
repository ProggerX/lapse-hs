import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Lapse (impureVal, list, numList, runExpression)
import Lapse.Infix (toInfix)
import Lapse.Operators
import Lapse.Types (Func, ScopeM, Value (..))

showTests :: [(Value, String)]
showTests =
  [ (numList [1, 2, 3], "(1 2 3)")
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

opTests :: [(ScopeM Value, Value)]
opTests =
  [ (ladd Nil, Number 0)
  , (lmul Nil, Number 1)
  , (ladd $ numList [1, 2, 3], Number 6)
  , (lsub $ numList [5, 3], Number 2)
  , (lmul $ numList [1, 2, 3, 4], Number 24)
  , (ldiv $ numList [55, 11], Number 5)
  , (lgrt $ numList [7, 2], Number 1)
  , (lgrt $ numList [2, 7], Nil)
  , (lgrt $ numList [5, 5], Nil)
  , (llss $ numList [7, 2], Nil)
  , (llss $ numList [2, 7], Number 1)
  , (llss $ numList [5, 5], Nil)
  , (leql $ numList [7, 2], Nil)
  , (leql $ numList [2, 7], Nil)
  , (leql $ numList [5, 5], Number 1)
  ]

condTests :: [(Value, Value)]
condTests =
  [ (Nil, Nil)
  , (list [list [Number 1, Number 2], list [Number 2, Number 3], list [Number 3, Number 4]], Number 2)
  , (list [list [Nil, Number 2], list [Number 2, Number 3], list [Number 3, Number 4]], Number 3)
  , (list [list [Nil, Number 2], list [Nil, Number 3], list [Number 3, Number 4]], Number 4)
  , (list [list [Nil, Number 2], list [Nil, Number 3], list [Nil, Number 4]], Nil)
  ]

exprTests :: [(String, String)]
exprTests =
  [ ("(+ 1 2)", "[3]")
  , ("(let ((a 1)) a)", "[1]")
  , ("(let ((a 1) (b 2) (c 3)) '(,a ,b ,c ,(+ a b c)))", "[(1 2 3 6)]")
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
      , testGroup
          "operators"
          $ map (\(t, x) -> testCase "test" $ impureVal t @?= x) opTests
      , testGroup
          "cond"
          $ map (\(t, x) -> testCase "test" $ (impureVal . cond) t @?= x) condTests
      , testGroup
          "expression tests"
          $ map (\(t, x) -> testCase "test" $ runExpression t @?= x) exprTests
      ]
