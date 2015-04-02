import Control.Monad (unless)
import Text.XML.HXT.Core (runX, (>>>))
import Text.HandsomeSoup (css, parseHtml, (!))
import Data.List (isInfixOf)
import Network.HTTP (getRequest, rspBody, Response)
import Network.Browser (browse, request, setOutHandler)
import Network.URI (URI)

main :: IO ()
main = searchLoop

searchLoop :: IO ()
searchLoop = do
    putStr "Enter word to search: "
    word <- getLine
    unless (null word) $ do
        result <- lookupPronunciation word
        case result of
            NotFound -> putStrLn $ word ++ " not found"
            PronNotAvailable -> putStrLn $ "Pronunication not available for " ++ word
            Found url -> do
                putStrLn url
                downloadMp3 url
        searchLoop

data SearchResult = NotFound
                  | PronNotAvailable
                  | Found String -- Url of mp3
                  deriving (Show)

-- | Search word in dictionary, download the page and extract Pron URL from it
lookupPronunciation :: String -> IO SearchResult
lookupPronunciation word = do
    (rspUrl, rsp) <- getDictionaryPage (searchUrl word)
    if "spellcheck" `isInfixOf` show rspUrl
        then return NotFound
        else do
            let dictionaryHtmlSrc = rspBody rsp
            mp3UrlList <- extractPronUrls dictionaryHtmlSrc
            case mp3UrlList of
                []         -> return PronNotAvailable
                (mp3Url:_) -> return $ Found mp3Url

-- | Submit get request to dictionary url.
-- Using browse (not simpleHTTP) because Mc Millan dictionary uses redirects
getDictionaryPage :: String -> IO (URI, Response String)
getDictionaryPage url = browse $ do
    setOutHandler . const $ return () -- surpress browser output to stdout
    request $ getRequest url

-- | Given HTML source of a dictionary page extract pronunciation URLs
extractPronUrls :: String -> IO [String]
extractPronUrls htmlSrc = runX $ parseHtml htmlSrc >>> css ".audio_play_button" ! "data-src-mp3"

-- | Construct search url for given word
searchUrl :: String -> String
searchUrl word = "http://www.macmillandictionary.com/search/british/direct/?q=" ++ word

downloadMp3 :: String -> IO ()
downloadMp3 _  = return () --TODO - download+rename usign wget or http get?
