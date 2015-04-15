{-- Tool to extract info about Java constructs (i.e. Classes, Interface, Annotations and Enums) from Javadoc --}
import Control.Arrow ((&&&), (>>>))
import Control.Monad (void)
import Data.Char (isAlpha, isUpper, toUpper)
import Data.Function (on)
import Data.List (isPrefixOf, sort, groupBy, intercalate)
import System.Environment (getArgs)
import System.FilePath ((</>), takeBaseName, dropExtension)
import Text.HandsomeSoup (css)
import Text.Printf (printf)
import Text.XML.HXT.Core (readDocument, withParseHTML, withWarnings, getText, runX, ArrowXml, XmlTree, getAttrValue, (//>), (>>.), IOStateArrow)
import Text.XML.HXT.XPath.Arrows (getXPathTrees)
import Text.XML.HXT.Arrow.XmlState.TypeDefs (IOSArrow)
-----
import Test.HUnit

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn "usage: jdex <path-to-javadoc-root-dir>"
        (rootJavadocDir:_) -> printSubclassTree rootJavadocDir "com.google.gwt.user.client.ui.Widget"

printSubclassTree :: FilePath -> String -> IO ()
printSubclassTree jdroot fcqn = do
    let startFile = jdroot </> fqcnToJdFile fcqn
--    putStrLn startFile
    subclassLinks <- directKnownSubclasses startFile
    --print them
    mapM_ (\sublink -> putStrLn $ show (simpleClassName sublink) ++ " -> " ++ show(jdFileToSCN startFile)) subclassLinks
    -- continue recursively for each subclass
    mapM_ (printSubclassTree jdroot . jdFileToFQCN . lFile) subclassLinks

analyze :: FilePath -> IO ()
analyze javadocRootDir = do
    entries <- sort `fmap` processIndex javadocRootDir
    putStrLn "----- All items from index -----" >> mapM_ print entries
    putStrLn "----- Index Summary -----"
    print $ summarizeIndex entries
    putStrLn "---- TODO -----"
    mapM_ (processJavadoc javadocRootDir) $ filter isClass entries

-- | Info extractable from javadoc html link, that points to a Java construct javadoc html file.
-- The meaning of items is: (construct, path relative to html file relative to javadoc root dir, True when it's link within local filesystem, False otherwise)
data Link = Link
    { lConstruct :: Construct -- the construct this link points to
    , lFile :: FilePath       -- file path this link points to
    , lIsLocal :: Bool        -- True if this links shows path in local filesystem, False otherwise (i.e. points to some http://)
    } deriving (Show, Eq, Ord)

simpleClassName :: Link -> String
simpleClassName = jdFileToSCN . lFile

data Construct = Class | Interface | Annotation | Enum deriving (Show, Read, Eq, Ord)

isClass, isInterface, isAnnotation, isEnum :: Link -> Bool
isClass      = (Class ==)      . lConstruct
isInterface  = (Interface ==)  . lConstruct 
isAnnotation = (Annotation ==) . lConstruct
isEnum       = (Enum ==)       . lConstruct 

getIndexFile :: FilePath -> FilePath
getIndexFile rootJavadocDir = rootJavadocDir </> "allclasses-noframe.html"

-- Javadoc parsing functionality
parseHtmlFile :: FilePath -> IOStateArrow s b XmlTree
parseHtmlFile = readDocument [withParseHTML True, withWarnings False]


extractInfo :: FilePath -> IOSArrow XmlTree a -> IO [a]
extractInfo file arrow = do
    let parsedFile = parseHtmlFile file
    runX $ parsedFile >>> arrow

processIndex :: FilePath -> IO [Link]
processIndex javadocRootDir = extractInfo (getIndexFile javadocRootDir) getLinks

directKnownSubclasses :: FilePath -> IO [Link]
directKnownSubclasses javadocFile = extractInfo javadocFile subclassesArrow
    where subclassesArrow = getXPathTrees "//h2/following-sibling::dl/dt/b[contains(text(),'Direct Known Subclasses')]/../../dd" >>> getLinks

-- Arrow to extract class/interface/enum/annotation info from javadoc index file's "a" elements:
-- e.g: <a title="class in com.google.gwt.core.ext" href="path/to/javadoc/file.html" ... will be mapped to ("class", "path/to/javadoc/file.html", True)
getLinks :: ArrowXml a =>  a XmlTree Link
getLinks = css "a" >>> (getAttrValue "title" &&& getAttrValue "href") >>. map parseTitleAndHref
  where 
    parseTitleAndHref :: (String, String) -> Link
    parseTitleAndHref (title, href) = Link (extractConstruct title) href (not $ "http" `isPrefixOf` href)

    extractConstruct :: String -> Construct
    extractConstruct title = read . (\x -> toUpper (head x) : tail x) $ takeWhile (/=' ') title

summarizeIndex :: [Link] -> [(Construct, Int)] -- how many pieces of each construct (class, interface, annotation, enum) does the list contain?
summarizeIndex = map ((lConstruct . head) &&& length) . groupBy ((==) `on` lConstruct) . sort

processJavadoc :: FilePath -> Link -> IO ()
processJavadoc jdRoot (Link construct file isLocal) = do
    let jdFile = jdRoot </> file
        doc = parseHtmlFile jdFile
    putStrLn $ printf "%s %s (%s)" (show construct) (jdFileToFQCN file) jdFile
    subsectionHeadings <- runX $ doc >>> (getXPathTrees directKnownSublcasses //> getText)
    mapM_ putStrLn subsectionHeadings
  where
    --subsectionHeadingXP = "//div[@class='description']//dt" -- new
    subsectionHeadingXP = "//h2/following-sibling::dl/dt/b" --odler javadoc versions TODO -find out which
  --linksUnderSubsection heading = subsectionHeadingXP ++ "[contains(text(),'" ++ heading ++ "')]/following-sibling::dd/a"
    linksUnderSubsection heading = subsectionHeadingXP ++ "[contains(text(),'" ++ heading ++"')]/../../dd/a" --older version of javadoc
    directKnownSublcasses = linksUnderSubsection "Direct Known Subclasses"


-- For each construct contains the list of all subsecion headings that can appear in its javadoc
construct2JDSections :: [(String, [String])]
construct2JDSections = [
    ("interface", ["All Known Implementing Classes", "All Known Subinterfaces", "All Superinterfaces", "Enclosing class", "Enclosing interface"]),
    ("class", ["All Implemented Interfaces", "Direct Known Subclasses", "Enclosing class", "Enclosing interface"]),
    ("enum", ["All Implemented Interfaces", "Enclosing class", "Enclosing interface"]),
    ("annotation", [])
    ]

jdFileToFQCN :: FilePath -> String -- javadoc html file name to fully qualified class name (like "java.lang.String")
jdFileToFQCN = replace '/' '.' . dropStartingDots . dropExtension
    where dropStartingDots = dropWhile (not . isAlpha)

--TODO improve performance - can't just replace all '.' with '/' because of nested classes like org.my.Outer.Inner -> org/my/Outer.Inner.html
fqcnToJdFile :: String -> FilePath -- Convert fully qualified class name to its corresponding javadoc html file
fqcnToJdFile = (++ ".html") . addPathSlashes . splitPkgCls
    where addPathSlashes (pkgs, clss) = intercalate "/" pkgs ++ "/" ++ intercalate "." clss
          splitPkgCls = break (isUpper . head) . words . replace '.' ' '

jdFileToSCN :: FilePath -> String -- Javadoc file path (java/lang/String.html) to simple ClassName, like "String"
jdFileToSCN = takeBaseName


replace :: Char -> Char -> String -> String
replace x y = map (\z -> if z == x then y else z)


---- TESTS ----
runAllTests :: IO ()
runAllTests = void $ runTestTT allTests

allTests = TestList 
    [ jdFileToFQCN "org/graphstream/ui/swingViewer/LayerRenderer.html" ~?= "org.graphstream.ui.swingViewer.LayerRenderer"
    , jdFileToFQCN "com/google/gwt/user/datepicker/client/CellGridImpl.Cell.html" ~?= "com.google.gwt.user.datepicker.client.CellGridImpl.Cell"

    , jdFileToSCN "org/graphstream/ui/swingViewer/LayerRenderer.html" ~?= "LayerRenderer"
    , jdFileToSCN "com/google/gwt/user/datepicker/client/CellGridImpl.Cell.html" ~?= "CellGridImpl.Cell"

    , fqcnToJdFile "java.lang.String" ~?= "java/lang/String.html"
    , fqcnToJdFile "com.google.gwt.user.datepicker.client.CellGridImpl.Cell" ~?= "com/google/gwt/user/datepicker/client/CellGridImpl.Cell.html"
    , fqcnToJdFile "org.graphstream.ui.swingViewer.LayerRenderer" ~?= "org/graphstream/ui/swingViewer/LayerRenderer.html"
    ]
