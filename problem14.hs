import Data.List

collatz :: Int -> [Int]
collatz n 
  | n == 1 = [1]
  | odd n = n : (collatz (3*n+1))
  | even n = n : (collatz (n `div` 2))
collatzes = map (length . collatz) [1..1000000]
maxCollatz = maximum collatzes
answer = case (elemIndex maxCollatz collatzes) of Just x -> x + 1