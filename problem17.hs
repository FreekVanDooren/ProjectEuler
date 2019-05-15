singles = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
doubles = ["twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]

toWords n
  | n == 1000 = "onethousand"
  | n >= 100 && n `mod` 100 == 0 = singles!!(n `div` 100 - 1) ++ "hundred"
  | n >= 100 = singles!!(n `div` 100 - 1) ++ "hundredand" ++ toWords (n `mod` 100)
  | n >= 20 = doubles!!(n `div` 10 - 2) ++ toWords (n `mod` 10)
  | n > 0 && n < 10 = singles!!(n - 1)
  | n == 0 = ""
  | n == 10 = "ten"
  | n == 11 = "eleven"
  | n == 12 = "twelve"
  | n == 13 = "thirteen"
  | n == 14 = "fourteen"
  | n == 15 = "fifteen"
  | n == 16 = "sixteen"
  | n == 17 = "seventeen"
  | n == 18 = "eighteen"
  | n == 19 = "nineteen"

answer = sum $ map (length . toWords) [1..1000]