module ListOps
  ( length
  , reverse
  , map
  , filter
  , foldr
  , foldl'
  , (++)
  , concat
  ) where

import Prelude hiding
  ( length, reverse, map, filter, foldr, (++), concat )

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f = foldr (flip f)

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ res [] = res
foldr f init (x:xs) = foldr f (f x init) xs

length :: [a] -> Int
length = foldr (\_ len -> len + 1) 0

reverse :: [a] -> [a]
reverse = r' []
  where
    r' acc [] = acc
    r' acc (x:xs) = r' (x:acc) xs

map :: (a -> b) -> [a] -> [b]
map f xs = [f x | x <- xs]

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter predicate (x:xs)
  | predicate x = (x:filter predicate xs)
  | otherwise = filter predicate xs

(++) :: [a] -> [a] -> [a]
xs ++ ys = error "You need to implement this function."

concat :: [[a]] -> [a]
concat = error "You need to implement this function."
