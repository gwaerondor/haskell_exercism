module Garden
    ( Plant (..)
    , defaultGarden
    , garden
    , lookupPlants
    ) where

{-
  Uploading this even though it's not finished yet.
-}

import Data.Map (Map, fromList, (!))
import Data.List (lines, sort)

type PlantList = String
type Student = String
type Garden = Map Student [Plant]

data Plant = Clover
           | Grass
           | Radishes
           | Violets
           deriving (Eq, Show)


defaultGarden :: PlantList -> Garden
defaultGarden plantList = garden names plantList
  where
    names = ["Alice", "Bob", "Charlie", "David",
              "Eve", "Fred", "Ginny", "Harriet",
              "Ileana", "Joseph", "Kincaid", "Larry"]

garden :: [Student] -> PlantList -> Garden
garden students plantList = fromList [(s, plantsFor s plantList) | s <- students]

toPlant :: Char -> Plant
toPlant 'C' = Clover
toPlant 'G' = Grass
toPlant 'R' = Radishes
toPlant 'V' = Violets

plantsFor :: Student -> PlantList -> [Plant]
plantsFor name plantList
  | name == "Alice" = map toPlant $ getFrom 0
  | name == "Bob" = map toPlant $ getFrom 2
  | name == "Charlie" = map toPlant $ getFrom 4
  | name == "David" = map toPlant $ getFrom 6
  | name == "Eve" = map toPlant $ getFrom 8
  | name == "Fred" = map toPlant $ getFrom 10
  | name == "Ginny" = map toPlant $ getFrom 12
  | name == "Harriet" = map toPlant $ getFrom 14
  | name == "Ileana" = map toPlant $ getFrom 16
  | name == "Joseph" = map toPlant $ getFrom 18
  | name == "Kincaid" = map toPlant $ getFrom 20
  | name == "Larry" = map toPlant $ getFrom 22
  where 
    getFrom n = concat [map (s!!) [n, n+1] | s <- lines plantList]

lookupPlants :: Student -> Garden -> [Plant]
lookupPlants student garden = garden ! student
