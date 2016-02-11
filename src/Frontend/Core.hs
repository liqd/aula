{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module Frontend.Core
where

import Data.String.Conversions
import Data.Typeable
import Lucid
import Lucid.Base
import Network.Wai.Internal (Response(ResponseFile, ResponseBuilder, ResponseStream, ResponseRaw))
import Network.Wai (Middleware)


semanticDiv :: forall m a. (Monad m, ToHtml a, Typeable a) => a -> HtmlT m () -> HtmlT m ()
semanticDiv t = div_ [makeAttribute "data-aula-type" (cs . show . typeOf $ t)]

-- | 'serveDirectory' lets wai guess the mime type, and wai's guess is not good enough.  This
-- 'Middleware' solves that.  (Alternatively, we could clone serveDirectory and solve the problem
-- closer to its cause, but the current solution makes it easier to add other tweaks as the need
-- arises.)
aulaTweaks :: Middleware
aulaTweaks app req cont = app req $ \resp -> do cont $ f resp
  where
    f :: Response -> Response
    f (ResponseFile s hs fp mfpart) = ResponseFile s (g <$> hs) fp mfpart
      where
        g ("content-type", "text/html") = ("content-type", "text/html;charset=utf8")
        g h = h
    f r@(ResponseBuilder _ _ _) = r
    f r@(ResponseStream _ _ _) = r
    f r@(ResponseRaw _ _) = r
