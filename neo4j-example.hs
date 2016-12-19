{-# LANGUAGE OverloadedStrings #-}

import Database.Neo4j
import qualified Data.HashMap.Lazy as M
import qualified Data.Text as T
import Data.Int

main = withAuthConnection "127.0.0.1" 7474 ("neo4j", "neo") $ do
   neo <- createNode someProperties
   cypher <- createNode M.empty
   r <- createRelationship "KNOWS" M.empty neo cypher
   return ()

someProperties = M.fromList
    [ "mytext" |: ("mytext" :: T.Text)
    , "textarrayprop" |: ["a" :: T.Text, "", "adeu"]
    , "int" |: (-12 :: Int64)
    , "intarray" |: [1 :: Int64, 2]
    , "double" |: (-12.23 :: Double)
    , "doublearray" |: [0.1, -12.23 :: Double]
    , "bool" |: False
    , "aboolproparray" |: [False, True]
    ]
