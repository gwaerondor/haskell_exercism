module Allergies (Allergen(..), allergies, isAllergicTo) where

import Data.Bits (testBit)

data Allergen = Eggs
              | Peanuts
              | Shellfish
              | Strawberries
              | Tomatoes
              | Chocolate
              | Pollen
              | Cats
              deriving (Eq)

allergies :: Int -> [Allergen]
allergies score = filter ((flip isAllergicTo) score) allergens 
  where
    allergens = [Eggs, Peanuts, Shellfish, Strawberries,
                 Tomatoes, Chocolate, Pollen, Cats]

isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo Eggs score = testBit score 0
isAllergicTo Peanuts score = testBit score 1
isAllergicTo Shellfish score = testBit score 2
isAllergicTo Strawberries score = testBit score 3
isAllergicTo Tomatoes score = testBit score 4
isAllergicTo Chocolate score = testBit score 5
isAllergicTo Pollen score = testBit score 6
isAllergicTo Cats score = testBit score 7
