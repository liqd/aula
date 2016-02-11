{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

{-# OPTIONS_GHC -Werror -Wall #-}

module Frontend.Core
where

import Data.Typeable
import Data.String.Conversions
import Lucid
import Lucid.Base


semanticDiv :: forall m a. (Monad m, ToHtml a, Typeable a) => a -> HtmlT m () -> HtmlT m ()
semanticDiv t = div_ [makeAttribute "data-aula-type" (cs . show . typeOf $ t)]
