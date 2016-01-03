{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Main where

import WdUtil
import Test.WebDriver
import Test.WebDriver.Commands.Wait
import Data.Text.IO
import Data.Text (Text)

scrapeProofWikiDefinitions :: WD Text
scrapeProofWikiDefinitions = do
  openPage "https://en.wikipedia.org/wiki/Category:Fields_of_mathematics"
  waitWhile 1500 $ clickElem (ByCSS ".CategoryTreeToggle[title=expand]")
  getSource

main :: IO ()
main = do
  pageSource <- doInChrome scrapeProofWikiDefinitions
  Data.Text.IO.writeFile "cats" pageSource

