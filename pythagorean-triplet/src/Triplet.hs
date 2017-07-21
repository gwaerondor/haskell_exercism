module Triplet (isPythagorean, mkTriplet, pythagoreanTriplets) where

import Data.List (sort, nub)

isPythagorean :: (Int, Int, Int) -> Bool
isPythagorean (a, b, c)
  | aa + bb == cc = True
  | aa + cc == bb = True
  | bb + cc == aa = True
  | otherwise = False
  where 
  aa = a * a
  bb = b * b
  cc = c * c

mkTriplet :: Int -> Int -> Int -> (Int, Int, Int)
mkTriplet a b c = (a, b, c)

pythagoreanTriplets :: Int -> Int -> [(Int, Int, Int)]
pythagoreanTriplets minFactor maxFactor = filterUnique $ [triplet | x <- [minFactor .. maxFactor],
                                                                    y <- [minFactor .. maxFactor],
                                                                    z <- [minFactor .. maxFactor],
                                                                    let triplet = mkTriplet x y z,
                                                                    isPythagorean triplet]

filterUnique triplets = toTupleList $ nub $ map sort $ toDeepList triplets
  where
    toDeepList ts = [[a, b, c] | (a, b, c) <- ts]
    toTupleList ls = [(a, b, c) | [a, b, c] <- ls]
