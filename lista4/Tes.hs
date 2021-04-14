import Control.Concurrent
import Control.Monad
import System.IO
import Text.Printf

main = do
    m <- newEmptyMVar
    forkIO $ putMVar m 'x'
    r <- takeMVar m
    print r
