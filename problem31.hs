two_pounds = 200
coinPermutations :: [Int] -> Int -> Int
coinPermutations [_] _  = 1
coinPermutations (currentCoin:remCoins) amount = sum $ map withCoin [0..amount `div` currentCoin]
  where
    withCoin n = coinPermutations remCoins (amount - n*currentCoin)
answer = coinPermutations [two_pounds,100,50,20,10,5,2,1] two_pounds