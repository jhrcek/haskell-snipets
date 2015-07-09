module Showdot (showDot) where 
import System.Process (runCommand, waitForProcess)
import System.IO (openTempFile, hPutStr, hClose)
import Control.Monad (void)

showDot :: String -> IO ()
showDot dot = do
    tmpFile <- writeToTempFile dot 
    runCmd $ "dot " ++ tmpFile ++ " -Tpng -o a.png"
    runCmd "shotwell a.png"
 where runCmd cmdStr = runCommand cmdStr >>= void . waitForProcess 

writeToTempFile :: String -> IO FilePath
writeToTempFile str = do
    (tmpFile, tmpHandle) <- openTempFile "." "dot.tmp"
    putStrLn $ "Creating temp file " ++ tmpFile
    hPutStr tmpHandle str
    hClose tmpHandle
    return tmpFile
