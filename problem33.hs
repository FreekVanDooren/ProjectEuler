import Data.List (foldl1',foldr,delete,unfoldr, length, foldr1)
import Data.Tuple (swap)

asInt = foldl1' (\a b -> 10*a+b)

reverseDigits = unfoldr takeLastInt
  where takeLastInt x | x == 0 = Nothing
                      | otherwise = Just . swap $ x `divMod` 10

deleteAll items list = foldr delete list items

numbersUpTo limit = filter (/=0) $ unfoldr next 11
  where next x | x >= limit || x >= 100 = Nothing
               | x `mod` 10 == 0 = Just (0,x+1) -- 0-hack is ugly. Open for suggestions
               | x == reverseLimit = Just (0,x+1)
               | otherwise = Just (x, x+1)
        reverseLimit = asInt . reverseDigits $ limit

fractions = [(a,b) |
    denom <- numbersUpTo 100,
    num <- numbersUpTo denom,
    let denomDigs = reverseDigits denom,
    let numDigs = reverseDigits num,
    let aDigs = deleteAll denomDigs numDigs,
    let bDigs = deleteAll numDigs denomDigs,
    let a = asInt aDigs,
    let b = asInt bDigs,
    length aDigs == 1,
    (fromIntegral a) / (fromIntegral b)==(fromIntegral num)/(fromIntegral denom)]

answer = (fromIntegral b)/(fromIntegral $ gcd a b)
  where (a,b) = foldr1 (\(a,b) (a',b')-> (a*a',b*b')) fractions