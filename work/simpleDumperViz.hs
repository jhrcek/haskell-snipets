import Data.List 

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

renderItems :: ([String],[String],[String],[String]) -> [String]
renderItems (cs,is,es,as) =  
   ["node[shape=box]" ] ++ map show cs ++ 
   ["node[shape=ellipse]"] ++ map show is


parseEdges :: [String] -> [String]
parseEdges lns = snd $ foldl' addEdge ([],[]) lns
  where
    addEdge (from, edgs) line
        | "    " `isPrefixOf` line = let to = getName2 line in (from, (renderEdge from to):edgs)
        | otherwise                = (getName line, edgs)

renderEdge :: String -> String -> String
renderEdge from to = show from ++ "->" ++ show to

renderGraph :: (String -> Bool) -> String -> String
renderGraph lineFilter dump = unlines $
    ["strict digraph G {\nrankdir=BT"] ++
    renderItems (parseItems lns) ++
    parseEdges lns ++
    ["}"]
    where lns = filter lineFilter $ lines dump

-- There are two types of lines:
--   top level lines that represent javadoc items
--   dependent items that represent stuff that top level item points to

-- extract name from top level line
getName, getName2 :: String -> String
getName line = name
    where [construct,pkg,name] = words line

getName2 line = name
    where [verb,construct,pkg,name] = words line
