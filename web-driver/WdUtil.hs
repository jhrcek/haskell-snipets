{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module WdUtil
    ( clickElem
    , chromeConfig
    , setInput
    , withServer
    , doInChrome
    )
where

import Control.Concurrent (threadDelay)
import Control.Exception.Base (bracket)
import Control.Monad ((>=>))
import Data.Text (Text)
import System.Process (createProcess, shell, terminateProcess)
import Test.WebDriver
    ( Selector
    , WD
    , WDConfig
    , browser
    , chrome
    , clearInput
    , click
    , defaultCaps
    , defaultConfig
    , finallyClose
    , findElem
    , runSession
    , sendKeys
    , wdCapabilities
    )

clickElem :: Selector -> WD ()
clickElem = findElem >=> click

setInput :: Selector -> Text -> WD ()
setInput sel txt = do
    inp <- findElem sel
    clearInput inp
    sendKeys txt inp

chromeConfig :: WDConfig
chromeConfig =
    defaultConfig
        { wdCapabilities = defaultCaps{browser = chrome}
        }

doInChrome :: WD a -> IO a
doInChrome = withServer . runSession chromeConfig . finallyClose

{- Selenium server management -}

-- Run action with selenium server running in the background, and stop the server afte that
withServer :: IO a -> IO a
withServer action =
    bracket
        ( do
            (_, _, _, procHandle) <- createProcess $ shell "java -jar selenium-server-standalone-2.53.0.jar -Dwebdriver.chrome.driver=chromedriver"
            threadDelay 3000000 -- wait 3 s for server to start
            return procHandle
        )
        terminateProcess -- called with procHandle after action is done
        (const action)
