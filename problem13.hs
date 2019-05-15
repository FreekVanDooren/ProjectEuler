answer = do
  input <- readFile "problem13.txt"
  let nrs = map (\x -> read x :: Integer) $ lines input
  print $ "Nrs read: " ++ (show (length nrs))
  let first10OfSum = take 10 $ show $ sum nrs
  print $ "First 10 digits of sum: " ++ first10OfSum
