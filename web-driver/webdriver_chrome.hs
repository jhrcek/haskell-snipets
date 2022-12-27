#!/usr/bin/env stack
-- stack script --resolver lts-11.8 --package webdriver,text

import Control.Monad.IO.Class
import qualified Data.Text.IO as Txt
import Test.WebDriver

-- Prerequisites
-- 1. selenium server running in the background: `java -jar selenium-server-standalone-2.53.0.jar` (newer versions not supported by webdriver library)
-- 2. chromedriver in the working directory (or in other dir, but then config needs to point to it)

main :: IO ()
main = runSession config . finallyClose $ do
    openPage "https://mvnrepository.com/artifact/org.apache.camel/camel-test/2.18.2"
    src <- getSource
    liftIO $ Txt.writeFile "tmp.html" src

config :: WDConfig
config = useBrowser chrome defaultConfig
