module Triangle (rows) where

rows :: Int -> [[Integer]]
rows x = map row [0..(x-1)]

row :: Int -> [Integer]
row n = map toInteger $ map (n `binomial`) [0..n]

binomial n k = (factorial n) `div` ((factorial (n-k)) * (factorial k))

factorial 0 = 1
factorial n = n * (factorial $ n-1)
