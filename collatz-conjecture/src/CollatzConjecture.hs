module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n
  | n > 0 = Just (collatz' n 0)
  | otherwise = Nothing

collatz' :: Integer -> Integer -> Integer
collatz' 1 steps = steps
collatz' n steps
  | n `mod` 2 == 0 = collatz' (n `div` 2) (steps + 1)
  | otherwise = collatz' (n*3 + 1) (steps + 1)
