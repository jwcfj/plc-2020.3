main = do
  n <- readLn
  x <- getLine
  print $ remDiv (n :: Int) (words x)

remDiv :: Int -> [a] -> ([a], [a])
remDiv _ [] = ([], [])
remDiv n xs = ((take (n-1) xs), (drop n xs))
