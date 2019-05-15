nr = 600851475143
isqrt = floor . sqrt . fromIntegral
isPrime k = null [ x | x <- [2..isqrt k], k `mod` x == 0]
primeFactors n = [x | x<-2:[3,5..isqrt n], isPrime x, n `mod` x == 0]

answer = last $ primeFactors nr