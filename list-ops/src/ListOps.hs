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
foldl' _ res [] = res
foldl' f init (x:xs) = new `seq` (foldl' f new xs)
  where
    new = f init x
                       
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f init es = doFold f init (reverse es)
  where
    doFold _ res [] = res
    doFold f init (x:xs) = doFold f (f x init) xs
    
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
xs ++ ys = doAppend (reverse xs) ys
  where
    doAppend [] ys = ys
    doAppend (x:xs) ys = doAppend xs (x:ys)

concat :: [[a]] -> [a]
concat = foldr (\e acc -> (e ++ acc)) []
