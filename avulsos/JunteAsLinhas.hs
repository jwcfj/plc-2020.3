import System.IO
main = myLoop

myLoop = do done <- isEOF
            if done
              then print(output)
              else do line <- getLine
                      output++line
                      myLoop
        where output = ""