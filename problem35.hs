import Data.List (unfoldr, foldl1', foldr, splitAt)
import Data.Char (digitToInt)

isqrt = floor . sqrt . fromIntegral
isPrime k = null [ x | x <- [2..isqrt k], k `mod` x == 0]

digits = map digitToInt . show

asInt = foldl1' (\a b -> 10*a+b)

circles 11 = [11] --should do my due diligence, I know
circles x = map asInt . map rotate $ [1..length list]
  where
    list = digits x
    rotate n = y ++ x where (x,y) = splitAt n list

primes = [x | x<-[2..10^6], isPrime x]

circularPrimes = foldr step [] primes
  where step p acc | isCircularPrime = p : acc
                   | otherwise = acc
          where isCircularPrime = all isPrime $ circles p

answer = length circularPrimes