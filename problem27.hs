import Data.List
import Data.Ord
-- https://lucidmanager.org/quadratic-primes-euler-problem-27/

isqrt = floor . sqrt . fromIntegral
isPrime k = null [ x | x <- [2..isqrt (abs k)], k `mod` x == 0]

-- n^2+an+b=y
-- n=0 -> isPrime b
-- n=1 -> isPrime (1+a+b) 
coefficient_n0= filter isPrime [-1000..1000]
coefficients=[(a,b)| b<-coefficient_n0, a<-[-999..999], abs a < abs b, isPrime (1+a+b)]
sequenceLength (a,b) = length . takeWhile isPrime . map quadratic $ [0..]
  where quadratic n = n^2 + a*n + b
answer = snd $ maximumBy (comparing fst) [(sequenceLength coefficient, a*b)| coefficient@(a,b)<-coefficients]