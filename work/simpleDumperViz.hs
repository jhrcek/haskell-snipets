import Data.List 
import GvRender

parseItems :: [String] -> ([String],[String],[String],[String])
parseItems lns = foldl' addItem ([],[],[],[]) topLvlItems
  where
    topLvlItems = filter (not . isPrefixOf "    ") lns
    addItem (cls,ifs,ens,annots) line = 
      let name = getName line in case words line of
      ("class":_)      -> (name:cls, ifs, ens, annots)
      ("interface":_)  -> (cls, name:ifs, ens, annots)
      ("enum":_)       -> (cls, ifs, name:ens, annots)
      ("annotation":_) -> (cls, ifs, ens, name:annots)
      _                -> error $ "unexpected item: " ++ line

parseEdges :: [String] -> [(String, String)]
parseEdges lns = snd $ foldl' addEdge ([],[]) lns
  where
    addEdge (from, edgs) line
        | "    " `isPrefixOf` line = let to = getName2 line in (from, (from,to):edgs)
        | otherwise                = (getName line, edgs)

-- There are two types of lines:
--   top level lines that represent javadoc items
--   dependent items that represent stuff that top level item points to

-- extract name from top level line
getName, getName2 :: String -> String
getName line = name
    where [construct,pkg,name] = words line

getName2 line = name
    where [verb,construct,pkg,name] = words line
