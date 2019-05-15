import Data.List (sort, foldl1', permutations)
import Data.Char (digitToInt)

isqrt = floor . sqrt . fromIntegral
--isPrime :: Num a => a -> Bool
isPrime 1 = False
isPrime k = null [ x | x <- [2..isqrt k], k `mod` x == 0]

concatToNum = foldl1' (\a b -> 10*a+b)

pandigitals = pans 9
  where
    pans n | n == 1 = [[1]]
           | otherwise = (permutations [1..n]) ++ pans (pred n)

answer = head . reverse . sort . filter isPrime . map concatToNum $ pandigitals