import Data.List (unfoldr)

dec2bin = reverse . unfoldr d2b
  where
    d2b 0 = Nothing
    d2b x = Just (x `rem` 2, x `quot` 2)

isPalindrome x = x == reverse x

limit = 10^6-1

-- only odd numbers, because those would end with 1 in base 2 and palindrome with starting 0 is not allowed
basePalindromes = [x | x<-[1,3..limit],isPalindrome (show x) && isPalindrome (dec2bin x)]

answer = sum basePalindromes