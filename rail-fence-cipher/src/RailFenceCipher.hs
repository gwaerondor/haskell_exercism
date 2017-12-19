module RailFenceCipher (encode, decode) where
import Data.Map (Map, empty, member, toAscList, adjust, insert)
data Direction = Down | Up

encode :: Int -> String -> String
encode rails message = construct $ encoded
  where
    encoded = encode' rails 1 message Down (empty :: Map Int String)

encode' :: Int -> Int -> String -> Direction -> Map Int String -> Map Int String
encode' _ _ [] _ result = result
encode' rails currentRail (c:cs) direction result = encode' rails nr cs nd updated
  where
    nd = nextDir currentRail rails direction
    nr = nextRail currentRail nd
    updated = updateResult c currentRail result

nextDir :: Int -> Int -> Direction -> Direction
nextDir currentRail maxRails direction
  | currentRail == 1 = Down
  | currentRail == maxRails = Up
  | otherwise = direction

nextRail :: Int -> Direction -> Int
nextRail currentRail Up = currentRail - 1
nextRail currentRail Down = currentRail + 1
  
construct :: Map Int String -> String
construct m = toString (toAscList m)

toString :: [(Int, String)] -> String
toString [] = []
toString ((_, s):t) = s ++ (toString t)

updateResult :: Char -> Int -> Map Int String -> Map Int String
updateResult c rail m
  | member rail m = adjust (++ [c]) rail m
  | otherwise = insert rail [c] m

------------------------------------------------------------
decode :: Int -> String -> String
decode rails enc = decode' rails enc 1
  where
    cycleLength = (rails * 2) - 2
    width = length enc
    startedCycles = width `div` cycleLength
    start = take startedCycles enc
    end = "unfinished!"

every :: Int -> Int -> [a] -> [a]
every _ [] = []
every n xs = (head xs):(every n (drop n xs))
