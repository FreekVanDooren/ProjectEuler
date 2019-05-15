import Data.List (foldl1', sort)
import Data.Char (digitToInt)

concatToNum = foldl1' (\a b -> 10*a+b)

splitNum = map (fromIntegral . digitToInt) . show

pandigital x = pan' 1 []
  where
    pan' n acc | ((length acc) >= 9) = acc
               | otherwise = pan' (n+1) (acc++(splitNum (x*n)))


pandigitals = map concatToNum . filter ((==[1..9]) . sort) . map pandigital

answer = maximum . pandigitals $ [1..9999]