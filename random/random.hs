import System.Random

main = do
  gen <- getStdGen
  putStrLn "10 random letters from a to z:"
  print . take 10 $ randomRs ('a','z') gen
  putStrLn "3 random numbers from 1 to 100:"
  print . fst $ randomR (1::Int,100) gen
  print . fst $ randomR (1::Int,100) (mkStdGen 7) -- alternatively create generator from fixed seed
  rn <- randomRIO (1::Int,100) -- alternatively use global random number generator
  print rn
  putStrLn "Random Int (range not specified):"
  print (fst $ random (mkStdGen 1) :: Int)
  putStrLn "Random Bool"
  print (fst $ random (mkStdGen 1) :: Bool)
