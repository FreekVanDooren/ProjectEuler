isqrt = floor . sqrt . fromIntegral
isPrime k = null [ x | x <- [2..isqrt k], k `mod` x == 0]
primes = take 10002 [x | x<-[1..], isPrime x]
answer = last primes