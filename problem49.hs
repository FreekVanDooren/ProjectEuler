import Data.List (sort)

sortedNumbers = sort . show

isPrime 1 = False
isPrime k = case (primeFactors k) of
    (_:_:_) -> False -- more than 1 element, so not prime. Not using length, because this traverses the entire list. So this should be faster?
    _       -> True
primes = 2 : filter isPrime [3,5..]

primeFactors n = factor n primes
  where factor _ [] = []
        factor m (p:ps) | p*p > m        = [m]
                        | m `mod` p == 0 = p : factor (m `div` p) primes
                        | otherwise      = factor m ps

primes4digits = filter isPrime [1000..9999]

arithmeticPrimes = [show a ++ show b ++ show c |
  a <- primes4digits,
  b <- filter (\x -> x > a && sortedNumbers a == sortedNumbers x) primes4digits,
  let c = 2 * b - a,
  c `elem` primes4digits,
  sortedNumbers a == sortedNumbers c]