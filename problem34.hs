import Data.Char (digitToInt)

digits = map digitToInt . show
fac = product . enumFromTo 1
digitFactorial = sum . map fac . digits
upperLimit = 100000 --sum . map fac $ drop 3 [1..9]
digitFactorials=[ x |
  x<-[3..upperLimit],
  x == digitFactorial x]

answer = sum digitFactorials