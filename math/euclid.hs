import Text.Printf (printf, PrintfArg)
import Control.Monad (unless)

euclid :: (Integral a, PrintfArg a, Show a) => a -> a -> IO ()
euclid x y
    | x < y     = euclid y x
    | otherwise = do
        let (k, rem) = divMod x y
        putStrLn $ printf "%d = %d * %d + %d" x k y rem
        if rem == 0
           then putStrLn $ " -> gcd = " ++ show y
           else euclid y rem
              

