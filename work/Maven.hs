module Maven (parseModuleStructure) where

import Text.XML.HXT.Core (runX, (/>), (//>), readDocument, hasName, getText)
import System.FilePath ((</>))
import Data.Tree

type ModuleName = String

parseModuleStructure :: FilePath -> IO (Tree ModuleName)
parseModuleStructure mvnProjRoot = unfoldTreeM getModuleNames mvnProjRoot

-- looks for pom.xml in given dir
getModuleNames :: FilePath -> IO (ModuleName, [FilePath])
getModuleNames dir = do
    let doc = readDocument [] pom
    [artifactId] <- runX $ doc /> hasName "project" /> hasName "artifactId" //> getText
    modules <- runX $ doc //> hasName "modules" /> hasName "module" //> getText
    return (artifactId, map (dir </> ) modules)
  where
    pom = dir </> "pom.xml"
