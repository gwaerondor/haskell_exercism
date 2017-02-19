module ETL (transform) where

import Data.Map (Map, empty, insertWith, toList, fromList)
import Data.Char (toLower)

transform :: Map Int String -> Map Char Int
transform m = fromList
              $ swap
              $ concat
              $ [ individualEntries p cs | (p, cs) <- toList m ]
  where
    individualEntries p cs = [ (p, toLower c) | c <- cs ]
    
swap :: [(a, b)] -> [(b, a)]
swap es = [(y, x) | (x, y) <- es]
