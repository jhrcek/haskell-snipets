import System.Environment
import System.Console.GetOpt

data Dict = Duden | Dwds | Seznam deriving Show

data Action = Add | Download Dict | Verify deriving Show

flags =
  [ Option ['d'] ["download"] (OptArg (Download . parseDict) "duden|dwds|seznam")
      "Dictionary to download from"
  , Option ['v'] ["verify"]   (NoArg Verify)
      "Anki database verification"
  , Option ['a'] ["add"]      (NoArg Add)
      "Add downloaded pronunciation words to anki database"
  ]

parseDict d = case d of 
   (Just "duden")  -> Duden
   (Just "seznam") -> Seznam
   _               -> Dwds


parseAndPrintArgs :: [String] -> IO ()
parseAndPrintArgs args =
    case getOpt Permute flags args of
        --usually pattern machted on this tripple to differentiate valid / invalid combinations of cmd line args
        everything@(parsedOptsList, nonOpts, errorList) -> print everything
    
main = do
  getArgs >>= parseAndPrintArgs
  putStrLn $ "\n And you can print usage message: " ++ usageInfo "bla" flags
