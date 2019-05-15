addMax x y z = x + max y z

maxPathSum triangle = head $ foldr1 step triangle
  where step currentRow nextRow = zipWith3 addMax currentRow nextRow (tail nextRow)

answer fileName = do 
  input <- readFile fileName
  let triangle = (map.map) (\x -> read x :: Integer) $ map words $ lines input
  print $ "Max path of " ++ (take (length fileName - 4) fileName) ++ " is: " ++ (show $ maxPathSum triangle)

answer_18 = answer "problem18.txt"

answer_67 = answer "problem67.txt"