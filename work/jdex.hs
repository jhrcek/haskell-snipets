{-- Tool to extract info about Java constructs (i.e. Classes, Interface, Annotations and Enums) from Javadoc --}
import Control.Arrow ((&&&))
import Control.Monad (when, mapM_, void)
import Data.Char (isAlpha, isUpper)
import Data.Function (on)
import Data.List (isPrefixOf, sort, groupBy)
import Data.Maybe (fromJust)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.FilePath ((</>))
import Text.HandsomeSoup (css)
import Text.Printf (printf)
import Text.XML.HXT.Core
import Text.XML.HXT.XPath.Arrows (getXPathTrees)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn "usage: jdex <path-to-javadoc-root-rootJavadocDir>"
        (rootJavadocDir:_) -> analyze rootJavadocDir

analyze :: FilePath -> IO ()
analyze rootJavadocDir = do
    entries <- sort `fmap` processIndex rootJavadocDir
--    putStrLn "----- All items from index -----" >> mapM_ print entries
    putStrLn "----- Index Summary -----"
    print $ summarizeIndex entries
    putStrLn "---- TODO -----"
    mapM_ (processJavadoc rootJavadocDir) entries

-- | Info extractable from javadoc html link, that points to a Java construct javadoc html file.
-- The meaning of items is: (construct, path relative to html file relative to javadoc root dir, True when it's link within local filesystem, False otherwise)
type Link = (String, FilePath, Bool)

isClass, isInterface, isAnnotation, isEnum :: Link -> Bool
isClass      = fsteq "class"
isInterface  = fsteq "interface"
isAnnotation = fsteq "annotation"
isEnum       = fsteq "enum"

fsteq :: Eq a => a -> (a, b, c) -> Bool
fsteq x (a, _, _) = a == x

getIndexFile :: FilePath -> FilePath
getIndexFile rootJavadocDir = rootJavadocDir </> "allclasses-noframe.html"

processIndex :: FilePath -> IO [Link]
processIndex rootJavadocDir = do
    let parsedIndex = parseHtmlFile $ getIndexFile rootJavadocDir
    runX $ parsedIndex >>> getLinks

-- Arrow to extract class/interface/enum/annotation info from javadoc index file's "a" elements:
-- e.g: <a title="class in com.google.gwt.core.ext" href="path/to/javadoc/file.html" ... will be mapped to ("class", "path/to/javadoc/file.html", True)
getLinks :: ArrowXml a =>  a XmlTree Link
getLinks = css "a" >>> ((getAttrValue "title" >>^ extractConstruct) &&& getAttrValue "href") >>. map addLocalityInfo
  where 
    extractConstruct :: String -> String
    extractConstruct title = takeWhile (/=' ') title

    addLocalityInfo :: (String, FilePath) -> (String, FilePath, Bool) -- add True/False representing whether href is in local filesystem or http
    addLocalityInfo (construct, href) = (construct, href, not $ "http" `isPrefixOf` href)

summarizeIndex :: [Link] -> [(String, Int)] -- how many pieces of each construct (class, interface, annotation, enum) does the list contain?
summarizeIndex = map ((fst3 . head) &&& length) . groupBy ((==) `on` fst3) . sort
  where fst3 (x,_,_) = x

data DocumentedItem = Interface | Class | Enum | Annotation 
           
processJavadoc :: FilePath -> Link -> IO ()
processJavadoc jdRoot (construct, file, isLocal) = do
    let jdFile = jdRoot </> file
        doc = parseHtmlFile jdFile
    putStrLn $ printf "%s %s (%s)" construct (jdFileToFQCN file) jdFile
    subsectionHeadings <- runX $ doc >>> (getXPathTrees subsectionHeadingXP //> getText)
    mapM_ putStrLn subsectionHeadings
  where
    --subsectionHeadingXP = "//div[@class='description']//dt" -- new
    subsectionHeadingXP = "//h2/following-sibling::dl/dt/b" --odler javadoc versions TODO -find out which
  --linksUnderSubsection heading = subsectionHeadingXP ++ "[contains(text(),'" ++ heading ++ "')]/following-sibling::dd/a"
    linksUnderSubsection heading = subsectionHeadingXP ++ "[contains(text(),'" ++ heading ++"')]/../../dd/a" --older version of javadoc
    allImplementingClasses = linksUnderSubsection "All Known Implementing Classes"

-- For each construct contains the list of all subsecion headings that can appear in its javadoc
construct2JDSections :: [(String, [String])]
construct2JDSections = [
    ("interface", ["All Known Implementing Classes", "All Known Subinterfaces", "All Superinterfaces", "Enclosing class", "Enclosing interface"]),
    ("class", ["All Implemented Interfaces", "Direct Known Subclasses", "Enclosing class", "Enclosing interface"]),
    ("enum", ["All Implemented Interfaces", "Enclosing class", "Enclosing interface"]),
    ("annotation", [])
    ]

parseHtmlFile :: FilePath -> IOStateArrow s b XmlTree
parseHtmlFile = readDocument [withParseHTML yes, withWarnings no]

jdFileToFQCN :: FilePath -> String -- Extract fully qualified class (like "java.lang.String") from javadoc link
jdFileToFQCN file = take (length noDots - 5) {- drop ".html" -} . replace '/' '.' $ noDots
    where noDots = dropWhile (not . isAlpha) file

jdFileToSCN :: FilePath -> String -- Simple ClassName, like "String"
jdFileToSCN = dropWhile (not . isUpper) . jdFileToFQCN

replace :: Char -> Char -> String -> String
replace _ _ [] = []
replace x y (z:zs) | x == z     = y:replace x y zs
                   | otherwise  = z:replace x y zs
