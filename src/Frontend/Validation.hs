{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall -Werror #-}

-- TODO: Make it as a structure.
module Frontend.Validation
    ( module TP
    , Frontend.Validation.validate
    , Frontend.Validation.validateOptional
    , (<??>)
    , FieldParser
    , satisfies
    , manyNM
    )
where

import Text.Digestive as TD
import Text.Parsec as TP
import Text.Parsec.Error

import Frontend.Prelude

type FieldParser a = Parsec String () a

-- FIXME: Use (Error -> Html) instead of toHtml
fieldValidation :: String -> FieldParser a -> String -> TD.Result (Html ()) a
fieldValidation name parser value =
    either (TD.Error . toHtml . errorString) TD.Success $ parse (parser <* eof) name value
  where
    -- TODO: Remove '\n' from the error messages
    errorString e = unwords [sourceName $ errorPos e, "-", errorMsgs $ errorMessages e]
    -- TODO: Translation :)
    -- errorMsgs = showErrorMessages "or" "unknown" "excepting" "unexpected" "end of input"
    errorMsgs = showErrorMessages "vagy" "ismeretlen" "ideillo" "nem vart" "sztring vege"

validate :: (Monad m) => String -> FieldParser a -> Form (Html ()) m String -> Form (Html ()) m a
validate n p = TD.validate (fieldValidation n p)

validateOptional :: (Monad m) => String -> FieldParser a -> Form (Html ()) m (Maybe String) -> Form (Html ()) m (Maybe a)
validateOptional n p = TD.validateOptional (fieldValidation n p)

-- Missing from Parsec.
infix 0 <??>

-- Set the given message if the parser fails as an error message, pretend
-- no input is consumed.
(<??>) :: ParsecT s u m a -> String -> ParsecT s u m a
p <??> msg = TP.try p <?> msg

satisfies :: (a -> Bool) -> ParsecT s u m a -> ParsecT s u m a
satisfies predicate parser = do
    x <- parser
    unless (predicate x) $ fail ""
    return x

-- | Try to apply the given parser minimum 'n' and maximum 'm' times.
manyNM
    :: forall s u m a t . (Stream s m t)
    => Int -> Int -> ParsecT s u m a -> ParsecT s u m [a]
manyNM n m p = do
    let d = m - n
    xs <- replicateM n p
    ys <- run d []
    pure $ xs <> ys
  where
    run :: Int -> [a] -> ParsecT s u m [a]
    run 0 xs = return (reverse xs)
    run l xs = optionMaybe (TP.try p) >>= \case
                    Just x -> (run (l-1) (x:xs))
                    Nothing -> run 0 xs
