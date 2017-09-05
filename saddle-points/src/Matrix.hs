module Matrix (saddlePoints) where

import Data.Array
import Data.List (filter)

saddlePoints :: Ix i => Ord e => Array (i, i) e -> [(i, i)]
saddlePoints matrix = filter (isSaddlePoint matrix) (indices matrix)

isSaddlePoint :: Ix i => Ord e => Array (i, i) e -> (i, i) -> Bool
isSaddlePoint matrix ix
  | (e `maxIn` row) && (e `minIn` col) = True
  | otherwise = False
  where
    e = (matrix ! ix)
    row = getRow (fst ix) matrix
    col = getCol (snd ix) matrix
    
getRow :: Ix i => i -> Array (i, i) e -> [e]
getRow row matrix = map snd $ filter r (assocs matrix)
  where
    r x = (fst $ fst x) == row

getCol :: Ix i => i -> Array (i, i) e -> [e]
getCol col matrix = map snd $ filter c (assocs matrix)
  where
    c x = (snd $ fst x) == col

maxIn :: Ord a => a -> [a] -> Bool
e `maxIn` es = all (e >=) es

minIn :: Ord a => a -> [a] -> Bool
e `minIn` es = all (e <=) es
