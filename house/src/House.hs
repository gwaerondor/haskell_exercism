module House (rhyme) where

import Data.List ((!!))

rhyme = strip $ concat ["\nThis is " ++ (line x) | x <- allIndices]
  where
    strip = dropWhile (=='\n')
    allIndices = [0..((length thingsWithActions)-1)]

thingsWithActions = [("house that Jack built", ""),
                     ("malt", "lay in"),
                     ("rat", "ate"),
                     ("cat", "killed"),
                     ("dog", "worried"),
                     ("cow with the crumpled horn", "tossed"),
                     ("maiden all forlorn", "milked"),
                     ("man all tattered and torn", "kissed"),
                     ("priest all shaven and shorn", "married"),
                     ("rooster that crowed in the morn", "woke"),
                     ("farmer sowing his corn", "kept"),
                     ("horse and the hound and the horn", "belonged to")]

line i = "the " ++ thing ++ action
  where
    thing = fst (thingsWithActions !! i)
    action = case (snd (thingsWithActions !! i)) of
               "" -> ".\n"
               act -> "\nthat " ++ act ++ " " ++ line (i-1)
