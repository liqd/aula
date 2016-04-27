
-- {-# OPTIONS_GHC -Wall -Werror #-}

-- TODO: Make it as a structure.
module Frontend.Validation
    ( module TP
    , validation
    , (<??>)
    , FieldParser
    )
where

--import Data.String.Conversions (ST, cs)
import Text.Digestive as TD
import Text.Parsec as TP

import Frontend.Prelude

type FieldParser a = Parsec String () a

-- FIXME: Use (Error -> Html) instead of toHtml
fieldValidation :: FieldParser a -> String -> Result (Html ()) a
fieldValidation field t =
    either (TD.Error . toHtml . show) TD.Success $ parse field "" (cs t)

validation :: (Monad m) => FieldParser a -> Form (Html ()) m String -> Form (Html ()) m a
validation p = TD.validate (fieldValidation p)

infix 0 <??>

(<??>) :: ParsecT s u m a -> String -> ParsecT s u m a
p <??> msg = (TP.try p) <?> msg
