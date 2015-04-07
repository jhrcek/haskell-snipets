{-- Tool to extract info about Java constructs (i.e. Classes, Interface, Annotations and Enums) from Javadoc --}
import Control.Arrow ((&&&))
import Control.Monad (when, mapM_, void)
import Data.Char (isAlpha, isUpper)
import Data.Function (on)
import Data.List (isPrefixOf, sort, groupBy)
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
    entries <- processIndex rootJavadocDir
--    putStrLn "----- All items from index -----" >> mapM_ print entries
    putStrLn "----- Index Summary -----"
    print $ summarizeIndex entries
    putStrLn "---- TODO -----"
    mapM_ (processJavadoc rootJavadocDir) entries

type LinkedItem = (String, FilePath) -- Represents info extractable from one link in "allclasses-noframe.html" like ("class", "path/to/its/javadoc.html")

isClass, isInterface, isAnnotation, isEnum :: LinkedItem -> Bool
isClass      = fsteq "class"
isInterface  = fsteq "interface"
isAnnotation = fsteq "annotation"
isEnum       = fsteq "enum"

fsteq :: Eq a => a -> (a,b) -> Bool
fsteq x (a,_) = a == x

getIndexFile :: FilePath -> FilePath
getIndexFile rootJavadocDir = rootJavadocDir </> "allclasses-noframe.html"

processIndex :: FilePath -> IO [LinkedItem]
processIndex rootJavadocDir = do
    let parsedIndex = parseHtmlFile $ getIndexFile rootJavadocDir
    runX $ parsedIndex >>> getLinkedItems

-- Arrow to extract class/interface/enum/annotation info from javadoc index file's "a" elements:
-- <a title="class in com.google.gwt.core.ext" href="path/to/javadoc/file.html" ...
getLinkedItems :: ArrowXml a =>  a XmlTree LinkedItem
getLinkedItems = css "a" >>> ((getAttrValue "title" >>^ extractConstruct) &&& getAttrValue "href")
  where extractConstruct title = takeWhile (/=' ') title

summarizeIndex :: [LinkedItem] -> [(String, Int)] -- how many pieces of each construct (class, interface, annotation, enum) does the index contain?
summarizeIndex = map ((fst . head) &&& length) . groupBy ((==) `on` fst) . sort

data DocumentedItem = Interface | Class | Enum | Annotation 
           
processJavadoc :: FilePath -> LinkedItem -> IO ()
processJavadoc jdRoot (construct, file) = do
    let jdFile = jdRoot </> file
        doc = parseHtmlFile jdFile
    putStrLn $ printf "%s %s (%s)" construct (jdFileToFQCN file) jdFile
    subsectionHeadings <- runX $ doc >>> (getXPathTrees "//h2/following-sibling::dl/dt/b" //> getText)
    putStrLn $ "  " ++ show subsectionHeadings
{-    hrefs <- runX $ doc >>> getXPathTrees "//b[contains(text(),'All Superinterfaces')]/../../dd/a" >>> getAttrValue "href"
    let localHrefs = filter (not . isPrefixOf "http") hrefs
    mapM_ (\href -> putStrLn $ "\"" ++ jdFileToSCN file ++ "\" -> \"" ++ jdFileToSCN href ++ "\"") localHrefs-}
--runX $ parseHtml a >>> getXPathTrees "//b[contains(text(),'All Superinterfaces')]/../../dd/a"

parseHtmlFile :: FilePath -> IOStateArrow s b XmlTree
parseHtmlFile = readDocument [withParseHTML yes, withWarnings no]

jdFileToFQCN :: FilePath -> String -- Fully Qualified Class Name, like "java.lang.String"
jdFileToFQCN file = take (length noDots - 5) . replace '/' '.' $ noDots
    where noDots = dropWhile (not . isAlpha) file --part of jd filename after all dots in the name

jdFileToSCN :: FilePath -> String -- Simple ClassName, like "String"
jdFileToSCN file = dropWhile (not . isUpper) $ jdFileToFQCN file

replace :: Char -> Char -> String -> String
replace _ _ [] = []
replace x y (z:zs) | x == z     = y:replace x y zs
                   | otherwise  = z:replace x y zs
