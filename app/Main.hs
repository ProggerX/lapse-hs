module Main where

newtype Listik a = Listik (a, Maybe (Listik a))

lstToPrint :: (Show a) => Listik a -> String
lstToPrint (Listik (x, Nothing)) = show x
lstToPrint (Listik (x, Just xs)) = show x ++ lstToPrint xs

lstToPrint' :: (Show a) => Listik a -> Int -> String
lstToPrint' (Listik (x, Nothing)) n = "(" ++ show x ++ " . ()" ++ replicate n ')'
lstToPrint' (Listik (x, Just xs)) n = "(" ++ show x ++ " . " ++ lstToPrint' xs (n + 1)

printList :: (Show a) => Listik a -> IO ()
printList lst = putStrLn $ lstToPrint' lst 0

main :: IO ()
main = do
  let list = Listik (1, Just (Listik (2, Just (Listik (3, Nothing))))) :: Listik Int
  printList list
