import Data.List
lexicPerms = sort . permutations
answer =  (lexicPerms [0..9])!!(1000000-1)