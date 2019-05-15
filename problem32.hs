import Data.List (permutations, foldl1', foldr,delete)
import Data.Char (digitToInt)

sublists series = [z |
  (a,b)<-[(1,4),(2,3)], -- the product must always be 4 digits, therefore these can be the only 2 pairs
  let x = asInt $ take a series,
  let y = asInt $ take b $ drop a series,
  let z = asInt $ drop (a+b) series,
  x*y==z]

asInt = foldl1' (\a b -> 10*a+b)

unique = foldr uniq []
  where uniq x acc | x `elem` acc = acc
                   | otherwise = x:acc

answer1 = sum . unique . concat $ map sublists $ permutations [1..9]

--second attempt, minimizing problem space
answer2 = sum . unique $ problemSpace

problemSpace = [z |
  a<-(combinator 1 [] ++ combinator 2 []),
  c<-combinator 4 a, -- the product must always be 4 digits
  let x = asInt a,
  let z = asInt c,
  let (y,remainder) = z `divMod` x,
  remainder == 0,
  containsAll (digits y) (deleteAll (a++c) [1..9])
  ]

combinator::Int -> [Int] ->[[Int]]
combinator 1 without = map (:[]) $ deleteAll without [1..9]
combinator len without = foldr step [] $ combinator (pred len) without
  where step y acc = acc ++ (map (:y) $ deleteAll (y++without) [1..9])

deleteAll items list = foldr delete list items

--reverses order, but fine for this purpose
digits = map digitToInt . show

containsAll l1 l2 = null $ deleteAll l1 l2 ++ deleteAll l2 l1
