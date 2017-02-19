module Accumulate where

accumulate :: (a -> b) -> [a] -> [b]
accumulate f l = [(f x) | x <- l]
