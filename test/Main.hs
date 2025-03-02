import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Data.Functor.Identity (Identity)
import Lapse (evalLapseM, list, numList, runExpression')
import Lapse.Operators
import Lapse.Types (Func, LapseM, Value (..))

type PValue = Value Identity

showTests :: [(PValue, String)]
showTests =
  [ (numList [1, 2, 3], "(1 2 3)")
  , (Pair (Number 1) (Pair (Number 2) (Number 3)), "(1 2 . 3)")
  , (Pair (Pair (Number 1) (Number 2)) (Number 3), "((1 . 2) . 3)")
  , (Pair (Pair (Number 1) (Number 2)) (Pair (Number 3) Nil), "((1 . 2) 3)")
  , (Number 5, "5")
  , (Pair (Number 5) Nil, "(5)")
  ]

opTests :: [(LapseM Identity PValue, Identity PValue)]
opTests =
  [ (ladd Nil, pure $ Number 0)
  , (lmul Nil, pure $ Number 1)
  , (ladd $ numList [1, 2, 3], pure $ Number 6)
  , (lsub $ numList [5, 3], pure $ Number 2)
  , (lmul $ numList [1, 2, 3, 4], pure $ Number 24)
  , (ldiv $ numList [55, 11], pure $ Number 5)
  , (lgrt $ numList [7, 2], pure $ Number 1)
  , (lgrt $ numList [2, 7], pure Nil)
  , (lgrt $ numList [5, 5], pure Nil)
  , (llss $ numList [7, 2], pure Nil)
  , (llss $ numList [2, 7], pure $ Number 1)
  , (llss $ numList [5, 5], pure Nil)
  , (leql $ numList [7, 2], pure Nil)
  , (leql $ numList [2, 7], pure Nil)
  , (leql $ numList [5, 5], pure $ Number 1)
  ]

condTests :: [(PValue, Identity PValue)]
condTests =
  [ (Nil, pure Nil)
  , (list [list [Number 1, Number 2], list [Number 2, Number 3], list [Number 3, Number 4]], pure $ Number 2)
  , (list [list [Nil, Number 2], list [Number 2, Number 3], list [Number 3, Number 4]], pure $ Number 3)
  , (list [list [Nil, Number 2], list [Nil, Number 3], list [Number 3, Number 4]], pure $ Number 4)
  , (list [list [Nil, Number 2], list [Nil, Number 3], list [Nil, Number 4]], pure Nil)
  ]

exprTests :: [(String, Identity String)]
exprTests =
  [ ("(+ 1 2)", pure "[3]")
  , ("(let ((a 1)) a)", pure "[1]")
  , ("(let ((a 1) (b 2) (c 3)) '(,a ,b ,c ,(+ a b c)))", pure "[(1 2 3 6)]")
  , ("(let ((a \"stra\") (b \"bstr\")) (concat a b))", pure "[\"strabstr\"]")
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
          "operators"
          $ map (\(t, x) -> testCase "test" $ evalLapseM t @?= x) opTests
      , testGroup
          "cond"
          $ map (\(t, x) -> testCase "test" $ (evalLapseM . cond) t @?= x) condTests
      , testGroup
          "expression tests"
          $ map (\(t, x) -> testCase "test" $ runExpression' t @?= x) exprTests
      ]
