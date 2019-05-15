import Data.Set (fromList, toList)
isqrt = floor . sqrt . fromIntegral
factors n = (toList . fromList) $ 1: (concat [[x,n `div` x] | x<-[2..isqrt n], n `mod` x == 0])
factorsSum = sum . factors

upperLimit = 28123 - 1
isAbundant x = factorsSum x > x 
abundants = filter isAbundant [1..upperLimit]
isNonAbundantComposite n = null $ filter (\x -> (n-x) `elem` abundants) $ takeWhile (<=n `div` 2) abundants
answer = sum $ filter isNonAbundantComposite [1..upperLimit]