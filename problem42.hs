import Data.List.Split (splitOneOf)
import Data.Char (ord)


toList = filter (not . null) . splitOneOf "\","
base = ord 'A' - 1
letterScore = (subtract base) . ord
wordScore = sum . map letterScore
-- 1/2 * n * (n + 1) = x
-- transform to quadratic form (https://en.wikipedia.org/wiki/Quadratic_formula):
-- n^2  + n - 2x = 0
-- where a=1, b=1, and c= -2x
-- Therefore discrimant becomes:
-- 1 - 4 * 1 * (-2x)
-- 1 + 8x
-- This always produces an odd number therefore the rest of the quadratic formula will always produce an integer
-- This means the only thing to be tested is if the square root of the discrimant is an integer
isInt x = x == fromInteger (round x)
isTriangleValue x = isInt (sqrt (fromIntegral d))
  where d = 1 + 8*x

nrOfTriangleWords = length . filter isTriangleValue . map wordScore . toList

answer = do
  input <- readFile "problem42.txt"
  print $ "The number of triangle words is: " ++ (show . nrOfTriangleWords $ input)