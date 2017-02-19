module Sublist where
data Sublist = Sublist | Equal | Superlist | Unequal deriving (Show, Eq)

sublist :: Eq a => [a] -> [a] -> Sublist
sublist a b
  | a == b = Equal
  | a == [] = Sublist
  | length a > length b && b `contained_in` a = Superlist
  | length a < length b && a `contained_in` b = Sublist
  | otherwise = Unequal

contained_in :: Eq a => [a] -> [a] -> Bool
contained_in a b
  | (length b) < (length a) = False
  | otherwise = case a == (take (length a) b) of
                  True -> True
                  False -> (a `contained_in` (tail b))
