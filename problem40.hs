import Data.Char (digitToInt)

answer = product . map (\x->digitToInt $ a!!(10^x)) $ [0..6]
  where a = concat . map show $ [0..]