{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module Frontend.Middleware
where

import Control.Category ((.))
import Control.Exception (assert)
import Control.Monad.Reader (runReader)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Internal (Response(ResponseFile, ResponseBuilder, ResponseStream, ResponseRaw))
import Prelude hiding (log, (.))

import qualified Data.ByteString.Builder as Builder
import qualified Data.Text as ST

import AulaPrelude
import Frontend.Core
import Frontend.Prelude


catchHttpErrors :: Bool -> Middleware
catchHttpErrors devMode app req cont = app req $ \resp -> cont $ f resp
  where
    f :: Response -> Response
    f resp = case statusCode status of
                404                     -> responseBuilder status headers body404
                n | n >= 400 && n < 500 -> responseBuilder status headers body4xx
                n | n >= 500            -> responseBuilder status headers body5xx
                _                       -> resp
      where
        status  = responseStatus resp
        builder page =
                  Builder.byteString . cs
                . (`runReader` whereToGetTheLangValue) . renderTextT . toHtml
                $ PublicFrame page [] devMode

        body404 = builder Page404
        body4xx = builder (Page4xx $ statusCode status)
        body5xx = builder (Page5xx $ statusCode status)

        htmlContentType = ("Content-Type", "text/html;charset=utf-8")

        repairContentType =
            \case ("Content-Type", "text/plain") -> htmlContentType
                  h                              -> h

        addContentType hs
            | any (view (_1 . to (== "Content-Type"))) hs = hs
            | otherwise = htmlContentType : hs

        headers = addContentType . map repairContentType $ responseHeaders resp


-- | If query contains @create_page_sample=true@, set header @Accept: text/plain@.  This provides a
-- way to extract page samples to feed to @src/RenderHtml.hs@.
createPageSamples :: Middleware
createPageSamples app req = app req'
  where
    req' = case partition (== ("create_page_sample", Just "true")) $ queryString req of
        ([], _)      -> req
        ([_], query) -> req { queryString = query
                            , requestHeaders = ("Accept", "text/plain") : requestHeaders req
                            }
        bad -> assert False $ error ("createPageSamples: impossible: " <> show bad)


-- * Cache control

-- | Configure response caching in the browser. The wrapped handler can overwrite this by setting
-- its own cache control headers.
--
-- The policy argument is scanned from left to right for a matching path prefix, or a nothing.  The
-- first match is used for this request.
--
-- Cache-control headers are only added to GET and HEAD responses since other request methods
-- are considered uncacheable.
--
-- According to the HTTP 1.1 Spec, GET/HEAD responses with the following error codes (>= 400) may
-- be cached unless forbidded by cache-control headers:
--
-- * 404 Not Found
-- * 405 Method Not Allowed
-- * 410 Gone
-- * 414 Request-URI Too Long
-- * 501 Not Implemented
cacheControlHeader :: [(ST, ResponseHeaders)] -> Middleware
cacheControlHeader policy app req cont = app req $
    cont . if cacheRelevantMethod (requestMethod req)
        then maybe id addHeadersToResponse matchingPolicy
        else id
  where
    matchingPolicy :: Maybe ResponseHeaders
    matchingPolicy = firstJust $ f <$> policy
      where
        f (prefix, plc) =
            if prefix `ST.isPrefixOf` (("/" <>) . ST.intercalate "/" . pathInfo $ req)
                then Just plc
                else Nothing

        firstJust :: [Maybe a] -> Maybe a
        firstJust = join . find isJust

cacheHeadersNoCache :: [(ST, ResponseHeaders)]
cacheHeadersNoCache =
    [(nil, [("Cache-Control", "no-cache, no-store, must-revalidate"), ("Expires", "0")])]

cacheHeadersCacheStatic :: [(ST, ResponseHeaders)]
cacheHeadersCacheStatic =
    ("/static", [("Cache-Control", "public")]) :
    ("/avatar", [("Cache-Control", "public, must-revalidate, max-age=10800")]) :
    cacheHeadersNoCache

cacheRelevantMethod :: Method -> Bool
cacheRelevantMethod = (`elem` ["GET", "HEAD"])

addHeadersToResponse ::  ResponseHeaders -> Response -> Response
addHeadersToResponse extraHeaders resp = case resp of
    ResponseFile status hdrs filepath part -> ResponseFile status (updH hdrs) filepath part
    ResponseBuilder status hdrs builder    -> ResponseBuilder status (updH hdrs) builder
    ResponseStream status hdrs body        -> ResponseStream status (updH hdrs) body
    ResponseRaw action resp'               -> ResponseRaw action $
                                                  addHeadersToResponse extraHeaders resp'
  where
    updH hdrs = nubBy ((==) `on` fst) $ extraHeaders <> hdrs
