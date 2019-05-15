import Data.List (tails, foldr, unfoldr, maximumBy, scanl1)

isPrime k | k <= 1 = False
          | otherwise = case (primeFactors k) of
                          (_:_:_) -> False -- more than 1 element, so not prime. Not using length, because this traverses the entire list. So this should be faster?
                          _       -> True
primes = 2 : filter isPrime [3,5..]

primeFactors n = factor n primes
  where factor _ [] = []
        factor m (p:ps) | p*p > m        = [m]
                        | m `mod` p == 0 = p : factor (m `div` p) primes
                        | otherwise      = factor m ps

-- produces tuples where first item is nr of primes that constitutes the sum value that is the second item.
highestConsecutiveSum from to = last . filter (\(_,y) -> isPrime y) . zip [1..] . takeWhile (<to) . scanl1 (+) $ dropWhile (<from) primes

-- maximum compares the first element in tuples.
highestConsecutiveSumBelow x = maximum [highestConsecutiveSum n x | n <- [2,3,5,7]] -- no proof for only taking first few primes as starting points, but no point to take one over 10. Empirically turns out to be true. And is much (!) faster than searching entire solution space.

answer = snd (highestConsecutiveSumBelow 1000000)

--100: 41 [13,11,7,5,3,2]
--1000: 953 [89,83,79,73,71,67,61,59,53,47,43,41,37,31,29,23,19,17,13,11,7]
--10000: 9521 [317,313,311,307,293,283,281,277,271,269,263,257,251,241,239,233,229,227,223,211,199,197,193,191,181,179,173,167,163,157,151,149,139,137,131,127,113,109,107,103,101,97,89,83,79,73,71,67,61,59,53,47,43,41,37,31,29,23,19,17,13,11,7,5,3]