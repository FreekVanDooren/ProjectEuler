import Data.Char

fac n = product [1..n]
factorialDigitSum = sum . map digitToInt . show . fac
answer=factorialDigitSum 100