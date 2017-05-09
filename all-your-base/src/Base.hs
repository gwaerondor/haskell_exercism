module Base (rebase, toDecimal) where

rebase :: Integral a => a -> a -> [a] -> Maybe [a]
rebase inputBase outputBase inputDigits = Just [1]

toDecimal :: Integral a => a -> [a] -> Maybe a
toDecimal inputBase digits
  | validNumber = Just (toDecimal' inputBase digits (length digits))
  | otherwise = Nothing
  where
    validNumber = all (<inputBase) digits

toDecimal' _ [] _ = 0
toDecimal' inputBase (d:ds) pos = (d * (inputBase^(pos-1))) + (toDecimal' inputBase ds (pos-1))
