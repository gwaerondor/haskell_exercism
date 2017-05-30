module Base (rebase, toDecimal) where

rebase :: Integral a => a -> a -> [a] -> Maybe [a]
rebase inputBase outputBase inputDigits
  | hasInvalidBase = Nothing
  | otherwise = do
      decimal <- (toDecimal inputBase inputDigits)
      Just (fromDecimal outputBase decimal)
  where hasInvalidBase = any (<= 1) [inputBase, outputBase]
    
toDecimal :: Integral a => a -> [a] -> Maybe a
toDecimal inputBase digits
  | inputBase < 2 = Nothing
  | validNumber = Just (toDecimal' inputBase digits (length digits))
  | otherwise = Nothing
  where
    validNumber = (all (<inputBase) digits) && (all (>= 0) digits)

toDecimal' :: Integral a => a -> [a] -> Int -> a
toDecimal' _ [] _ = 0
toDecimal' inputBase (d:ds) pos = (d * (inputBase^(pos-1))) + (toDecimal' inputBase ds (pos-1))

fromDecimal :: Integral a => a -> a -> [a]
fromDecimal outputBase dec = reverse $ fromDecimal' outputBase dec

fromDecimal' :: Integral a => a -> a -> [a]
fromDecimal' _ 0 = []
fromDecimal' outputBase dec = (dec `rem` outputBase):(fromDecimal' outputBase (dec `quot` outputBase))
