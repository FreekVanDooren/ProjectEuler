normal = [31,28,31,30,31,30,31,31,30,31,30,31]
leap = [31,29,31,30,31,30,31,31,30,31,30,31]

nr_of_months = 100 * 12

yearlyCycles = take nr_of_months (cycle $ concat [normal,normal,normal,leap])

startingDays :: [Integer] -> [Integer]
startingDays year = foldr step [2] $ reverse year
  where step monthLength days@(start:_) = ((start + monthLength) `mod` 7) : days

answer = length $ filter (==0) $ startingDays yearlyCycles