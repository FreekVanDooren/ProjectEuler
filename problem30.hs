import Data.Char (digitToInt)

summedPowerDigitSums power = sum . filter (\x -> sumOfPowerDigits x == x) $ [2..limit]
  where
    sumOfPowerDigits = sum . map (\x -> (digitToInt x)^power) . show
    limit = (* 9^power) . (+1) . head $ dropWhile (\x -> x^power > 10^(x-1)) [2..]

answer = summedPowerDigitSums 5