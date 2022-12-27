{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Monad (mzero)
import Data.Aeson (FromJSON, Value (..), decode, parseJSON, (.:))
import qualified Data.ByteString.Lazy.Char8 as BS

data Person = Person
    { name :: String
    , age :: Int
    }
    deriving (Show)

instance FromJSON Person where
    parseJSON (Object o) =
        Person
            <$> o .: "name"
            <*> o .: "age"
    parseJSON _ = mzero

main = print (decode $ BS.pack "{\"name\":\"John\", \"age\":25}" :: Maybe Person)
