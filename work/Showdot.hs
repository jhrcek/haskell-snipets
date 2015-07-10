module Showdot (showDot) where 
import System.Directory (removeFile)
import System.Process (runCommand, waitForProcess)
import System.IO (hPutStr,IOMode(WriteMode), withFile)
import Control.Monad (void)

showDot :: String -> IO ()
showDot dotStr = do
    writeToTempFile dotStr
    generateImage >> displayImage
    mapM_ removeFile ["tmp.dot", "a.png"]
 where
    generateImage = runCmd "dot -Tpng -o a.png tmp.dot"
    displayImage = runCmd "shotwell a.png"
    runCmd cmdStr = runCommand cmdStr >>= void . waitForProcess 

writeToTempFile :: String -> IO ()
writeToTempFile content = 
    withFile "tmp.dot" WriteMode $ \handle ->
       hPutStr handle content

