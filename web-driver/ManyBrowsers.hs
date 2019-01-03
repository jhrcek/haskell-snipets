module ManyBrowsers where

import Control.Monad
import Test.WebDriver
import Test.WebDriver.Commands (closeSession, openPage)
import Test.WebDriver.Config (defaultConfig)
import Test.WebDriver.Monad (WD, runSession, runWD)
import Test.WebDriver.Session (WDSession, getSession)
import Test.WebDriver.Session.History (SessionHistory (histRequest))

createSessionIO :: IO WDSession
createSessionIO = runSession (useBrowser chrome defaultConfig) getSession

doActionInManyBrowsers :: WD () -> IO ()
doActionInManyBrowsers action = do
  sessions <- replicateM 3 createSessionIO
  mapM_ (\s -> runWD s $ action >> closeSession) sessions
