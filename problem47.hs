import Data.List (group, groupBy)

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


findPrimeFactorsOf len = filter isOfLength . groupBy hasSamePrimeFactorLength $ [2..]
  where hasSamePrimeFactorLength x y = primeFactorLength x == primeFactorLength y
        primeFactorLength = length . group . primeFactors
        isOfLength factors@(x:xs) = length factors == len && primeFactorLength x == len 

answer = head . head $ findPrimeFactorsOf 4