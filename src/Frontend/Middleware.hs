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
                _            -> resp
      where
        status  = responseStatus resp
        builder page =
                  Builder.byteString . cs
                . (`runReader` whereToGetTheLangValue) . renderTextT . toHtml
                $ PublicFrame page [] devMode

        body404 = builder Page404
        body4xx = builder Page4xx
        body5xx = builder Page5xx

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

-- | Disable response caching. The wrapped handler can overwrite this by setting its own cache
-- control headers.
--
-- Cache-control headers are only added to GET and HEAD responses since other request methods
-- are considered uncachable by default.
--
-- According to the HTTP 1.1 Spec, GET/HEAD responses with the following error codes (>= 400) may
-- be cached unless forbidded by cache-control headers:
--
-- * 404 Not Found
-- * 405 Method Not Allowed
-- * 410 Gone
-- * 414 Request-URI Too Long
-- * 501 Not Implemented
disableCaching :: Middleware
disableCaching app req cont = app req $
    cont . if relevantMeth then addHeadersToResponse cacheHeaders else id
  where
    cacheHeaders =
        [ ("Cache-Control", "no-cache, no-store, must-revalidate")
        , ("Expires", "0")
        ]

    relevantMeth :: Bool
    relevantMeth = requestMethod req `elem` ["GET", "HEAD"]

addHeadersToResponse ::  ResponseHeaders -> Response -> Response
addHeadersToResponse extraHeaders resp = case resp of
    ResponseFile status hdrs filepath part -> ResponseFile status (updH hdrs) filepath part
    ResponseBuilder status hdrs builder    -> ResponseBuilder status (updH hdrs) builder
    ResponseStream status hdrs body        -> ResponseStream status (updH hdrs) body
    ResponseRaw action resp'               -> ResponseRaw action $
                                                  addHeadersToResponse extraHeaders resp'
  where
    updH hdrs = nubBy ((==) `on` fst) $ extraHeaders <> hdrs
