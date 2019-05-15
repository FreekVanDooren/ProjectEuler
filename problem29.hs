import Data.List (nub,group,sort)
original_answer = (length . nub) [a^b|a<-[2..100],b<-[2..100]]
faster_answer = length . group . sort $ [a^b|a<-[2..100],b<-[2..100]]