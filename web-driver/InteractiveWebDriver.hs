{-# LANGUAGE OverloadedStrings #-}

import Control.Monad ((>=>))
import Control.Monad.IO.Class (liftIO)
import Data.Maybe
import Data.Text (Text)
import Test.WebDriver.Commands (Selector (..), attr, clearInput, click, findElem, openPage, sendKeys)
import Test.WebDriver.Config (defaultConfig)
import Test.WebDriver.Monad (WD, finallyClose, getSessionHistory, runSession, runWD)
import Test.WebDriver.Session (WDSession, getSession)
import Test.WebDriver.Session.History (SessionHistory (histRequest))
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match
-- General Utilities

clickElem :: Selector -> WD ()
clickElem = findElem >=> click

getSessionIO :: IO WDSession
getSessionIO = runSession defaultConfig getSession

setInput :: Selector -> Text -> WD ()
setInput sel txt = do
  inp <- findElem sel
  clearInput inp
  sendKeys txt inp

--- Wikipedia Utilities

getContentHtml :: WD Text
getContentHtml = fmap (fromMaybe "") (findElem (ByCSS "#mw-content-text > .mw-parser-output") >>= (`attr`"innerHTML"))

-- Scenario

--scenario :: IO ()
scenario =
  runSession defaultConfig $ do
     openPage "https://en.wikipedia.org/wiki/Mathematics"
     html <- getContentHtml
     let preambleTags = extractPreamble $ parseTags html
     liftIO $ mapM_ print preambleTags
     return preambleTags

extractPreamble = takeWhile (not . tagOpenAttrLit "div" ("id","toc"))
