import Data.Char
import Data.List
import Data.List.Split

toSortedList = sort . filter (not . null) . splitOneOf "\","
base = ord 'A' - 1
letterScore = (subtract base) . ord
nameScore index name = (index *) . sum . map letterScore $ name
nameScores = zipWith nameScore [1..]
totalNameScore = sum . nameScores . toSortedList
answer = do
  input <- readFile "problem22.txt"
  print $ "The total of all the name scores is: " ++ (show $ totalNameScore input)
