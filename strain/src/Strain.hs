module Strain (keep, discard) where

-- Doing this without Data.List.filter and list comprehension generator guards.

discard :: (a -> Bool) -> [a] -> [a]
discard _ [] = []
discard p (x:xs)
  | p x = discard p xs
  | otherwise = (x:discard p xs)

keep :: (a -> Bool) -> [a] -> [a]
keep _ [] = []
keep p (x:xs)
  | p x = (x:keep p xs)
  | otherwise = keep p xs
