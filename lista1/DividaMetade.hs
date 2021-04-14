main = do
  x <- getLine
  print $ halve (words x)

halve :: [a] -> ([a], [a])
halve xs = ((takeImpar xs 1), (takePar xs 1))

takeImpar :: [a] -> Int -> [a]
takeImpar [] _ = []
takeImpar (x:xs) i
    |(mod i 2) == 0 = takeImpar xs (i+1)
    |otherwise = [x] ++ takeImpar xs (i+1)

takePar :: [a] -> Int -> [a]
takePar [] _ = []
takePar (x:xs) i
    |(mod i 2) == 0 = [x] ++ takePar xs (i+1)
    |otherwise = takePar xs (i+1)