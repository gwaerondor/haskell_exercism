module SecretHandshake (handshake) where
import Data.Bits ((.&.))

handshake :: Int -> [String]
handshake i = foldr (\f acc -> f i acc) [] actions
  where
    actions = reverse [wink, doubleBlink, closeEyes, jump, reverseOrder]

wink :: Int -> [String] -> [String]
wink i
  | isFlagSet 1 i = (++["wink"])
  | otherwise = id

doubleBlink :: Int -> [String] -> [String]
doubleBlink i
  | isFlagSet 2 i = (++["double blink"])
  | otherwise = id

closeEyes :: Int -> [String] -> [String]
closeEyes i
  | isFlagSet 4 i = (++["close your eyes"])
  | otherwise = id

jump :: Int -> [String] -> [String]
jump i
  | isFlagSet 8 i = (++["jump"])
  | otherwise = id

reverseOrder :: Int -> [String] -> [String]
reverseOrder i
  | isFlagSet 16 i = reverse
  | otherwise = id

isFlagSet :: Int -> Int -> Bool
isFlagSet f fs = f .&. fs == f
