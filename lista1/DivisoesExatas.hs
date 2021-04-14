main = do
   a <- readLn
   b <- readLn
   print (numDiv (a :: Int) (b :: Int))

numDiv:: Integral a => a -> a -> a
numDiv x 0 = 0
numDiv x y
    |x < y = 0
    |(mod x y) == 0 = 1 + (numDiv (div x y) y)
    |otherwise =  0