{-# LANGUAGE OverloadedStrings #-}
module Test.WebDriver.Missing
where

import Data.Aeson
import Data.String.Conversions
import Data.Text as ST

import Test.WebDriver
import Test.WebDriver.Class

-- | Get the data of the first image we find from out of the dom.  (It would be nice to pass the
-- result of 'findElem' to this function, but that's just a pointer into some webdriver dictionary,
-- and the json representation makes no sense in the context of the browser, so we just pass the
-- xpath to the image as string.)
--
-- credit: http://stackoverflow.com/questions/934012/get-image-data-in-javascript
jsGetBase64Image :: WebDriver wd => ST -> wd Value
jsGetBase64Image xpath = executeJS [JSArg xpath] $ ST.unlines
    [ "var arg0 = arguments[0];"
    , "var img = document.evaluate(arg0, document, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null).singleNodeValue;"
    , ""
    , "var canvas = document.createElement(\"canvas\");"
    , "canvas.width = img.width;"
    , "canvas.height = img.height;"
    , "canvas.getContext(\"2d\").drawImage(img, 0, 0);"
    , "return { \"value\": canvas.toDataURL(\"image/png\") };"
    ]
