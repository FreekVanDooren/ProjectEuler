solve = do
  input <- readFile "problem11.txt"
  let grid = map (\x -> read x :: Int) $ concat $ map words $ lines input
  putStrLn ("My grid is this long: " ++ (show $ length grid))
  let horis = [[grid!!(x), grid!!(x+1), grid!!(x+2), grid!!(x+3)]| x <- concat $ map (\x -> [x..x+16]) [0,20..380]]
  putStrLn ("My horis is this long: " ++ (show $ length horis))
  putStrLn ("My last horis is this: " ++ (show $ last horis))
  let verts = [[grid!!(x), grid!!(x+20), grid!!(x+40), grid!!(x+60)]| x <- [0..339]]
  putStrLn ("My verts is this long: " ++ (show $ length verts))
  putStrLn ("My last verts is this: " ++ (show $ last verts))
  let diagFor = [[grid!!(x), grid!!(x+21), grid!!(x+42), grid!!(x+63)]| x <- concat $ map (\x -> [x..x+16]) [0,20..339]]
  putStrLn ("My diagFor is this long: " ++ (show $ length diagFor))
  putStrLn ("My last diagFor verts is this: " ++ (show $ last diagFor))
  let diagBack = [[grid!!(x), grid!!(x+19), grid!!(x+38), grid!!(x+57)]| x <- concat $ map (\x -> [x..x+16]) [3,23..323]]
  putStrLn ("My diagBack is this long: " ++ (show $ length diagBack))
  putStrLn ("My last diagBack verts is this: " ++ (show $ last diagBack))
  let largestProduct = maximum $ map product $ horis ++ verts ++ diagFor ++ diagBack
  putStrLn ("The largest product on the grid is: " ++ (show largestProduct))