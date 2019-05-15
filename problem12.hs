isqrt = floor . sqrt . fromIntegral
factors n = [x | x<-[2..isqrt n], n `mod` x == 0]
triangleNumbers = scanl1 (+) [1..]
answer = head $ filter ((> 249).length.factors) $ triangleNumbers