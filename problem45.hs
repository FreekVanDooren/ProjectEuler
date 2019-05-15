isInt x = x == fromInteger (round x)
--see problem 42
isTriangleValue x = isInt (sqrt (fromIntegral d))
  where d = 1 + 8*x
--see problem 44
isPentagonicValue x = isInt y
  where
    d = 1 + 24*x
    y = (1 + sqrt (fromIntegral d))/6

answer = head [hexaValue |
  n <-[1..],
  let hexaValue = n*(2*n-1),
  isTriangleValue hexaValue,
  isPentagonicValue hexaValue,
  hexaValue > 40755]