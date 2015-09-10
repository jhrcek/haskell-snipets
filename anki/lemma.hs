{-# LANGUAGE OverloadedStrings #-}
import Data.Text
import Data.Text.IO (readFile)
import Prelude hiding (readFile, lookup, words, lines)
import Data.Map.Strict
import Control.Applicative ((<$>))

createDict :: IO (Text -> Maybe Text)
createDict = do  
    lns <- lines <$> readFile "lemmatization-de.txt"
    let partialDict = fromList $ fmap lineToEntry lns -- word to lemma (unfortunately does not contain mapping lemma -> lemma)
        l2l         = fromList $ fmap lineToLemmaEntry lns  -- lemma to lemma
        dictionary  = partialDict `union` l2l
    return $ \w -> lookup (toLower w) dictionary
  where
    lineToEntry = wsToEntry . words
    wsToEntry [lemma, w] = (toLower w, lemma)
    lineToLemmaEntry = (\(lemma:_) -> (toLower lemma, lemma)) . words

