import Data.List
import Data.Ord
--http://mathworld.wolfram.com/DecimalExpansion.html
reciprocalCycleLength n = recipLength 1 []
  where recipLength exp multOrd =
          let x = exp `mod` n
          in case elemIndex x multOrd of
            Just i -> length multOrd - start
            Nothing -> recipLength (exp*10) (x:multOrd)
        start | n `mod` 2 == 0 = calcOffset 2
              | n `mod` 5 == 0 = calcOffset 5
              | otherwise = 0
        calcOffset divisor = last $ takeWhile isPoweredDivisor [0..]
          where isPoweredDivisor x = let poweredDivisor = divisor^x in poweredDivisor < n && n `mod` poweredDivisor == 0

reciprocals = [(x, reciprocalCycleLength x)| x <- [1..999]]
answer = fst $ maximumBy (comparing snd) reciprocals