module Allergies (Allergen(..), allergies, isAllergicTo) where

import Data.Bits ((.&.))

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
isAllergicTo Eggs score = (1 .&. score) == 1
isAllergicTo _ _ = False
