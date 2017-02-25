module Hamming (distance) where

distance :: String -> String -> Maybe Int
distance xs ys
  | sameLength = Just $ length $ filter (\(x, y) -> x /= y) (zip xs ys)
  | otherwise = Nothing
  where
    sameLength = (length xs) == (length ys)

