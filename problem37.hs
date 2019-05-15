import Data.List (foldl1',tails, unfoldr)

isqrt = floor . sqrt . fromIntegral
isPrime 1 = False
isPrime k = null [ x | x <- [2..isqrt k], k `mod` x == 0]

asInt :: String -> Integer
asInt x = read x::Integer

concatToInt :: Num a => [a] -> a
concatToInt = foldl1' (\a b -> 10*a+b)

rightTruncate :: Integer -> [Integer]
rightTruncate x = reverse $ unfoldr dropLastDigit 10
  where dropLastDigit m | m > x = Nothing
                        | otherwise = Just(x `div` m, m*10)

leftTruncate :: Integer -> [Integer]
leftTruncate x = unfoldr dropFirstDigit 10
  where dropFirstDigit m | m > x = Nothing
                         | otherwise = Just(x `mod` m, m*10)

isTruncatablePrime x = all isPrime $ rightTruncate x ++ leftTruncate x

primes = 2: [x | x<-[3,5..], isPrime x]

truncatablePrimes = take 11 . filter isTruncatablePrime $ filter (>7) primes

answer = sum truncatablePrimes