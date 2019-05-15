-- 1/2 * n * (3n - 1) = x
-- transform to quadratic form (https://en.wikipedia.org/wiki/Quadratic_formula):
-- 3*n^2 - n - 2x = 0
-- where a=3, b=-1, and c= -2x
-- Therefore discrimant becomes:
-- 1 - 4 * 3 * (-2x)
-- 1 + 24x
isInt x = x == fromInteger (round x)
isPentagonic x = isInt y
  where
    d = 1 + 24*x
    y = (1 + sqrt (fromIntegral d))/6

pentagonics = [n*(3*n-1) `div` 2 | n<-[1..]]--up to 10000 there literally is only 1 answer

pentaDiffs = [(j,k,d) |
  j<- pentagonics,
  k<- takeWhile (<j) pentagonics,
  isPentagonic (j+k),
  let d = j-k,
  isPentagonic d]

answer = head pentaDiffs