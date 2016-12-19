import Data.Array.IO
import Data.IORef

main :: IO ()
main = do
  iorefDemo
  arrayDemo

iorefDemo :: IO ()
iorefDemo = do
    x <- newIORef 0
    a <- readIORef x :: IO Int
    writeIORef x 1
    b <- readIORef x
    print (a, b)

arrayDemo :: IO ()
arrayDemo = do
    ar <- newArray (1,10) 12 :: IO (IOArray Int Int)
    a <- readArray ar 1
    writeArray ar 1 34
    b <- readArray ar 1
    print (a, b)
