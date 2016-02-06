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
-- import Control.Monad.IO.Class
-- import Control.Monad.Trans.Except (ExceptT(ExceptT))
import Control.Monad (forM_)
-- import Data.Aeson (Value(String), ToJSON(toJSON), (.=), encode, object)
-- import Data.CaseInsensitive (CI, mk, foldCase, foldedCase)
-- import Data.List (nubBy)
-- import Data.Maybe (fromMaybe)
-- import Data.Proxy (Proxy(Proxy))
-- import Data.Set (Set)
import Data.String.Conversions
-- import Data.String.Conversions (SBS, ST, cs, (<>))
-- import Data.String (fromString)
-- import Data.Text.Encoding (decodeUtf8')
import Data.Typeable
-- import Data.Typeable (Typeable)
-- import Data.Void (Void, absurd)
-- import Network.HTTP.Types (Header, methodGet, methodHead, methodPost, ok200, statusCode)
-- import Network.Wai (Application, Middleware, Request, requestHeaders, requestMethod, responseHeaders, responseStatus)
-- import Network.Wai.Handler.Warp (runSettings, setHost, setPort, defaultSettings)
-- import Network.Wai.Internal (Response(ResponseFile, ResponseBuilder, ResponseStream, ResponseRaw))
-- import Servant
-- import Servant.API ((:>))
-- import Servant.API.ContentTypes (AllCTRender)
-- import Servant.HTML.Blaze
-- import Servant.Server
-- import Servant.Server.Internal
-- import Servant.Server.Internal.ServantErr
-- import Servant.Utils.Links (HasLink(MkLink, toLink), linkURI)
-- import Servant.Utils.StaticFiles
import Test.QuickCheck
import Text.Blaze
-- import System.IO
import System.Process
import System.Environment
import System.FilePath
import System.Directory
import Text.Blaze.Renderer.Pretty (renderMarkup)
import Text.Show.Pretty (ppShow)
import System.IO.Unsafe (unsafePerformIO)

-- import qualified Blaze.ByteString.Builder as Builder
-- import qualified Data.Binary as Binary
-- import qualified Data.ByteString.Char8 as SBS
-- import qualified Data.ByteString.Lazy.Char8 as LBS
-- import qualified Data.Set as Set
-- import qualified Data.Text as ST
-- import qualified Network.HTTP.Types.Header as HttpTypes
-- import qualified Text.Blaze.Html5 as H
-- import qualified Text.Blaze.Html5.Attributes as A

import Arbitrary ()
import Config
import Frontend.Html
-- import Types


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
-- FIXME: handle cli errors
-- FIXME: change working directory relative to the executable
main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--recreate"] -> recreateSamples
        ["--refresh"]  -> refreshSamples


-- | Remove existing samples and generate new ones.
recreateSamples :: IO ()
recreateSamples = do
    let path = Config.config ^. Config.htmlStatic </> "samples"
    createDirectoryIfMissing False path
    setCurrentDirectory path
    system "rm -f *.hs *.html"
    samplePages >>= mapM_ writeSample . zip [0..]
    refreshSamples
  where
    writeSample :: (Int, (TypeRep, String)) -> IO ()
    writeSample (ix, (typRep, valueRepShow)) = do
        let fn :: FilePath
            fn | ix < 100 = (reverse . take 3 . reverse $ "000" ++ show ix ++ "_")
                         ++ show' typRep

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
  where
    g :: forall a. (Read a, ToMarkup a) => Proxy a -> Maybe String
    g proxy = unsafePerformIO $ violate (f proxy s) `catch` (\(SomeException _) -> return Nothing)
      where
        violate s = length s `seq` return (Just s)

    f :: forall a. (Read a, ToMarkup a) => Proxy a -> String -> String
    f Proxy = renderMarkup . Frame . (read :: String -> a)
