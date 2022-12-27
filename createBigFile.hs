import qualified Data.ByteString.Builder as BU
import qualified Data.ByteString.Lazy as B
import System.IO

main :: IO ()
main = do
    let chunk = BU.lazyByteString $ B.pack (replicate (2 ^ 15) 0)
    withFile
        "/home/jhrcek/Tmp/big"
        WriteMode
        (\h -> BU.hPutBuilder h (mconcat (replicate (2 ^ 16) chunk)))
