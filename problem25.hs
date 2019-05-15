fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
nrOfDigits = length . show
answer = length $ takeWhile ((1000>).nrOfDigits) fibs