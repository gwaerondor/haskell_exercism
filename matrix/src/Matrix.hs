module Matrix
    ( Matrix
    , cols
    , column
    , flatten
    , Matrix.fromList
    , fromString
    , reshape
    , row
    , rows
    , shape
    , transpose
    ) where

import Data.Vector as V (Vector, fromList, toList, (!), length, concat)
import Data.List ((!!))

data Matrix a = Matrix [Vector a] deriving (Eq, Show)

cols :: Matrix a -> Int
cols (Matrix cs) = V.length $ head cs

column :: Int -> Matrix a -> Vector a
column x (Matrix cs) = V.fromList $ [c ! x | c <- cs]

flatten :: Matrix a -> Vector a
flatten (Matrix m) = V.concat m

fromList :: [[a]] -> Matrix a
fromList xss = Matrix $ map V.fromList xss

fromString :: Read a => String -> Matrix a
fromString xs = Matrix $ map V.fromList $ map parseElements $ lines xs
  where
    parseElements cs = map read $ words cs
    
reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape (cols, rows) (Matrix cs) = error "This isn't implemented yet"

row :: Int -> Matrix a -> Vector a
row x (Matrix m) = m !! x

rows :: Matrix a -> Int
rows (Matrix cs) = Prelude.length cs

shape :: Matrix a -> (Int, Int)
shape (Matrix cs) = (cols, rows cs)
  where
    cols = Prelude.length cs
    rows [] = 0
    rows cs = Prelude.length $ head cs

transpose :: Matrix a -> Matrix a
transpose matrix = Matrix $ [column n matrix | n <- [0..((cols matrix)-1)]]
