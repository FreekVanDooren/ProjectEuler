isqrt = floor . sqrt . fromIntegral
factors n = 1: (concat [[x,n `div` x] | x<-[2..isqrt n], n `mod` x == 0])
factorsSum = sum . factors
amicablePairsUnder n = [x | x <- [0..n], let fSum =factorsSum x, let reverseFSum = factorsSum fSum, x == reverseFSum, reverseFSum/=fSum ]
answer = sum $ amicablePairsUnder 10000