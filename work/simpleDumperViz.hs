import Data.List 
import GvRender


parseAll :: String -> ([Construct], [Construct], [Edge])
parseAll str = (classes, interfaces, parseInheritance inheritance)
  where
    (constructs, inheritance) = break (== '[') str
    cs = parseConstructs constructs
    classes = filter (\c -> ctype c == Class) cs
    interfaces = filter (\c -> ctype c == Interface) cs

parseInheritance :: String -> [Edge]
parseInheritance = read

parseConstructs :: String -> [Construct]
parseConstructs = map parseConstr . lines
  where
    parseConstr line = case words line of 
      ["CLASS",      pkg, name, id] -> Construct Class      pkg name (read id)
      ["INTERFACE",  pkg, name, id] -> Construct Interface  pkg name (read id)
      ["ENUM",       pkg, name, id] -> Construct Enum       pkg name (read id)
      ["ANNOTATION", pkg, name, id] -> Construct Annotation pkg name (read id)
      _                -> error $ "Unexpected line : " ++ line
