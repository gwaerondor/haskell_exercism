module Hamming (distance) where

distance :: String -> String -> Maybe Integer
distance xs ys
  | sameLength = Just (distance' xs ys)
  | otherwise = Nothing
  where
    sameLength = (length xs) == (length ys)

distance' :: String -> String -> Integer
distance' "" "" = 0
distance' (x:xs) (y:ys)
  | x == y = 0 + (distance' xs ys)
  | otherwise = 1 + (distance' xs ys)
