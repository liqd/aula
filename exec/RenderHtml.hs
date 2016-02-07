{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}

{-# OPTIONS_GHC -Werror -Wall #-}


module Main (main) where

import Data.Maybe
import Control.Exception
import Control.Lens ((^.))
import Control.Monad (forM_)
import Data.String.Conversions
import Data.Typeable
import Test.QuickCheck
import Text.Blaze
import System.Process
import System.Environment
import System.FilePath
import System.Directory
import Text.Blaze.Renderer.Pretty (renderMarkup)
import Text.Show.Pretty (ppShow)
import System.IO.Unsafe (unsafePerformIO)

import qualified Text.Blaze.Html5 as H

import Arbitrary ()
import Config
import Frontend.Html


samplePages :: IO [(TypeRep, String)]
samplePages = sequence
    [ f <$> (generate arbitrary :: IO PageComment)
    , f <$> (generate arbitrary :: IO PageComment)
    , f <$> (generate arbitrary :: IO PageComment)
    , f <$> (generate arbitrary :: IO PageIdea)
    , f <$> (generate arbitrary :: IO PageIdea)
    , f <$> (generate arbitrary :: IO PageIdea)
    ]
  where
    f :: (Typeable a, Show a, ToMarkup a) => a -> (TypeRep, String)
    f x = (typeOf x, terminatingShow x)

    terminatingShow :: (Show a) => a -> String
    terminatingShow x = if length s < n then s else error e
      where
        n = 1000000
        s = take n $ ppShow x
        e = "terminatingShow: " ++ s


-- | ...
--
-- FIXME: check out blaze-from-html package
-- FIXME: document
-- FIXME: change working directory relative to the executable
main :: IO ()
main = do
    args <- getArgs
    progName <- getProgName
    case args of
        ["--recreate"] -> recreateSamples
        ["--refresh"]  -> refreshSamples
        bad -> error $ progName ++ ": bad args " ++ show bad


-- | Remove existing samples and generate new ones.
recreateSamples :: IO ()
recreateSamples = do
    let path = Config.config ^. Config.htmlStatic </> "samples"
    createDirectoryIfMissing False path
    setCurrentDirectory path
    _ <- system "rm -f *.hs *.html"
    samplePages >>= mapM_ writeSample . zip [0..]
    refreshSamples
  where
    writeSample :: (Int, (TypeRep, String)) -> IO ()
    writeSample (ix, (typRep, valueRepShow)) = do
        let fn :: FilePath
            fn | ix < 100 = (reverse . take 3 . reverse $ "000" ++ show ix ++ "_")
                         ++ show' typRep
               | otherwise = assert False $ error "recreateSamples: impossible."

            show' :: (Show a) => a -> String
            show' = map f . show
              where
                f ' ' = '_'
                f c = c

        writeFile (fn <.> "hs")            $ valueRepShow
        writeFile (fn <.> "hs" <.> "html") $ "<pre>" <> valueRepShow <> "</pre>"


-- | Read existing samples and re-render the HTML.
refreshSamples :: IO ()
refreshSamples = do
    let path = Config.config ^. Config.htmlStatic </> "samples"
    setCurrentDirectory path

    -- read *.bin
    bins <- filter ((== ".hs") . takeExtension) <$> getDirectoryContents "."

    -- write *.html
    forM_ bins $ \fn -> do
        let fn' = dropExtension fn <.> ".html"
        readFile fn >>= writeFile fn' . dynamicRender

-- | Take a binary serialization and use current 'ToMarkup' instances for
dynamicRender :: String -> String
dynamicRender s = case catMaybes [ g (Proxy :: Proxy PageComment)
                                 , g (Proxy :: Proxy PageIdea)
                                 ] of
    (v:_) -> v
    [] -> assert False $ error "dynamicRender: impossible."
  where
    g :: forall a. (Read a, ToMarkup a) => Proxy a -> Maybe String
    g proxy = unsafePerformIO $ violate (f proxy s) `catch` (\(SomeException _) -> return Nothing)
      where
        violate s' = length s `seq` return (Just s')

    f :: forall a. (Read a, ToMarkup a) => Proxy a -> String -> String
    f Proxy = renderMarkup . H.toHtml . Frame . (read :: String -> a)
