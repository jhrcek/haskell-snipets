import System.IO
import Control.Exception.Base (bracket)
import Control.Monad (unless)

fileLoop :: IO()
fileLoop = do
    putStrLn "Do you want to [read] a file, [write] a file or [quit]?"
    cmd <- getLine
    case cmd of
        "quit"  -> return ()
        "read"  -> do doRead; fileLoop
        "write" -> do doWrite; fileLoop
        _       -> do putStrLn ("I don't understand the command " ++ cmd); fileLoop


doRead = do
    putStr "Enter a file name to read: "
    fn <- getLine
    contents <- readFile fn
    putStr contents


doWrite = do
    putStr "Enter file name to write: "
    fn <- getLine
    bracket (openFile fn WriteMode) hClose
            (\h -> do putStrLn "Enter text (dot on a line by itself to end):"
                      writeLoop h)


writeLoop :: Handle -> IO()
writeLoop h = do
    line <- getLine
    unless (line == ".") $ do
      hPutStrLn h line
      writeLoop h
