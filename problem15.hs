fac n = product [1..n]
binomial n k = fac n `div` ((fac k) * (fac (n-k)))
answer = binomial 40 20