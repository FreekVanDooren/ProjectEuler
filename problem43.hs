import Data.List (foldl1')

concatToNum = foldl1' (\a b -> 10*a+b)

pandigs = [ [d1,d2,d3,d4,d5,d6,d7,d8,d9,d10] |
  d2 <- [0..9],
  d3 <- filter (/=d2) [0..9],
  d4 <- filter (`notElem` [d2,d3]) [0,2,4,6,8],
  even (concatToNum [d2,d3,d4]),
  d5 <- filter (`notElem` [d2,d3,d4]) [0..9],
  (d3+d4+d5) `mod` 3 == 0,
  d6 <- filter (`notElem` [d2,d3,d4,d5]) [0,5],
  d7 <- filter (`notElem` [d2,d3,d4,d5,d6]) [0..9],
  (concatToNum [d5,d6,d7]) `mod` 7 == 0,
  d8 <- filter (`notElem` [d2,d3,d4,d5,d6,d7]) [0..9],
  (concatToNum [d6,d7,d8]) `mod` 11 == 0,
  d9 <- filter (`notElem` [d2,d3,d4,d5,d6,d7,d8]) [0..9],
  (concatToNum [d7,d8,d9]) `mod` 13 == 0,
  d10 <- filter (`notElem` [d2,d3,d4,d5,d6,d7,d8,d9]) [0..9],
  (concatToNum [d8,d9,d10]) `mod` 17 == 0,
  d1 <- filter (`notElem` [d2,d3,d4,d5,d6,d7,d8,d9,d10]) [0..9]]

answer = sum . map concatToNum $ pandigs