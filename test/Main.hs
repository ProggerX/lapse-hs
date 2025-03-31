import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Control.Monad.State (evalStateT)
import Data.Map.Strict (empty)
import Lapse (evalLapseM', list, numList, runExpression')
import Lapse.Eval (eval)
import Lapse.Modules (std)
import Lapse.Operators
import Lapse.Parser (parse)
import Lapse.Types (Func, LapseM, Scopes, Value (..))

showTests :: [(Value, String)]
showTests =
  [ (numList [1, 2, 3], "(1 2 3)")
  , (Pair (Number 1) (Pair (Number 2) (Number 3)), "(1 2 . 3)")
  , (Pair (Pair (Number 1) (Number 2)) (Number 3), "((1 . 2) . 3)")
  , (Pair (Pair (Number 1) (Number 2)) (Pair (Number 3) Nil), "((1 . 2) 3)")
  , (Number 5, "5")
  , (Pair (Number 5) Nil, "(5)")
  ]

opTests :: [(LapseM Value, Value)]
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
  , ("(let ((a \"stra\") (b \"bstr\")) (concat a b))", "[\"strabstr\"]")
  ]

st :: Scopes
st = [empty, std]

main :: IO ()
main =
  defaultMain $
    testGroup
      "all"
      [ testGroup "show" [testCase x $ show t @?= x | (t, x) <- showTests]
      , testGroup
          "operators"
          [ testCase (show x) $ evalLapseM' t >>= (@?= x)
          | (t, x) <- opTests
          ]
      , testGroup
          "cond"
          [ testCase (show x) $ evalLapseM' (cond t) >>= (@?= x)
          | (t, x) <- condTests
          ]
      , testGroup
          "expression tests"
          [ testCase x $ runExpression' t >>= (@?= x)
          | (t, x) <- exprTests
          ]
      ]
