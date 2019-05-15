import Data.Char

powerSum = sum . map digitToInt . show . (2^)

answer = powerSum 1000