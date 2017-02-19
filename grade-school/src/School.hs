module School (School, add, empty, grade, sorted) where
import Data.Map (Map, fromList, insertWith, toAscList, member, (!))
import Data.List (sort)
type School = Map Int [String]

add :: Int -> String -> School -> School
add gradeNum student school = insertWith (++) gradeNum [student] school

empty :: School
empty = (fromList []) :: School

grade :: Int -> School -> [String]
grade gradeNum school
  | gradeNum `member` school = sort $ school ! gradeNum
  | otherwise = []

sorted :: School -> [(Int, [String])]
sorted school = [(g, sort n) | (g, n) <- toAscList school]
