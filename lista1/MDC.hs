main = do
   a <- readLn
   b <- readLn
   print (mdc (a :: Int) (b :: Int))

mdc :: Int -> Int -> Int
mdc 0 y = y
mdc x 0 = x
mdc x y = mdc y (mod x y)