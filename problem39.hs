import Data.List (group, sort)

triangles p = [(a,b,c)|a<-[1..p],b<-[a..p], let c=p-b-a, a^2+b^2==c^2]

naive_answer = maximum . map (\x -> (length $ triangles x,x)) $ [1..1000]

--https://en.wikipedia.org/wiki/Pythagorean_triple
-- unique answers, multiples of these amount to another triangle. e.g. (a=3,b=4,c=5->p=12) *5 == (a=15,b=20,c=25,p=60)
euclidSpace = [p |
  n<-[1..20],--max 20, because with m=20 and n=21 -> a+b+c > 1000
  m<-[n+1..21],
  (even n && odd m) || (odd n && even m),
  gcd n m == 1,
  let a = m^2-n^2,
  let b = 2*m*n,
  let c = m^2+n^2,
  let p = a+b+c,
  p <= 1000]

multiplesUptil limit p = takeWhile (<=limit) . map (*p) $ [1..]

fullEuclidSpace = concat . map (multiplesUptil 1000) $ euclidSpace

mostCommonItem = snd . maximum . map (\xs -> (length xs, head xs)) . group . sort

euclidAnswer = mostCommonItem fullEuclidSpace