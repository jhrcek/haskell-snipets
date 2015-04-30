import Data.IORef -- Mutable references in the IO monad
import Data.Array.IO -- mutable boxed non-strict array in the IO monad

main = do
  iorefDemo
  arrayDemo

iorefDemo = do
    x <- newIORef 0  
    a <- readIORef x
    writeIORef x 1
    b <- readIORef x
    print (a, b)

arrayDemo = do
    ar <- newArray (1,10) 12 :: IO (IOArray Int Int)
    a <- readArray ar 1
    writeArray ar 1 34
    b <- readArray ar 1
    print (a, b)
