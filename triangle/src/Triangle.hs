module Triangle (TriangleType(..), triangleType) where

import Data.List ((\\))

data TriangleType = Equilateral
                  | Isosceles
                  | Scalene
                  | Illegal
                  deriving (Eq, Show)

triangleType a b c
  | illegal = Illegal
  | allEqual = Equilateral
  | twoEqual = Isosceles
  | noneEqual = Scalene
  where
    illegal = isIllegal [a, b, c]
    allEqual = (sidesEqual a b c) == 3
    twoEqual = (sidesEqual a b c) == 2
    noneEqual = (sidesEqual a b c) == 1

sidesEqual a b c = 1 + max (occurences a [b, c]) (occurences b [a, c])
  where
    occurences x = length . filter (==x)

isIllegal [0, 0, 0] = True
isIllegal sides = any (==True) [ (sumOfOthers x < x) | x <- sides ]
  where
    sumOfOthers x = (sum (sides \\ [x]))
