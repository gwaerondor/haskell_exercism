module Matrix (saddlePoints) where

import Data.Array
import Data.List (filter)

saddlePoints :: Ix i => Ord e => Array (i, i) e -> [(i, i)]
saddlePoints matrix = filter (isSaddlePoint matrix) (indices matrix)

isSaddlePoint :: Ix i => Ord e => Array (i, i) e -> (i, i) -> Bool
isSaddlePoint matrix ix
  | maxInRow && minInCol = True
  | otherwise = False
  where
    e = (matrix ! ix)
    row = getRow (fst ix) matrix
    col = getCol (snd ix) matrix
    maxInRow = all (e >=) row
    minInCol = all (e <=) col

getRow :: Ix i => i -> Array (i, i) e -> [e]
getRow index matrix = get index matrix (\x -> (fst $ fst x) == index)

getCol :: Ix i => i -> Array (i, i) e -> [e]
getCol index matrix = get index matrix (\x -> (snd $ fst x) == index)

get index matrix getter = map snd $ filter getter (assocs matrix)
