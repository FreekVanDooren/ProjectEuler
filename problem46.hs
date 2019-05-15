import Data.List (foldr, find)

isqrt = floor . sqrt . fromIntegral
isPrime 1 = False
isPrime k = null [ x | x <- [2..isqrt k], k `mod` x == 0]

primes = 2 : filter isPrime [3,5..]
notPrimes = filter (not . isPrime) [3,5..]

findCompositesPrime x = find isComposite $ takeWhile (< x) primes
  where isComposite p = let n = (x-p) `div` 2 in (isqrt n)^2 == n

answer = head . dropWhile ((/= Nothing) . findCompositesPrime) $ notPrimes