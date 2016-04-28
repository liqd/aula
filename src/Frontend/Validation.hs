{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Frontend.Validation
    ( module TP
    , FieldParser
    , Frontend.Validation.validate
    , Frontend.Validation.validateOptional
    , inRange
    , (<??>)
    , manyNM
    , satisfies
    )
where

import Text.Digestive as TD
import Text.Parsec as TP
import Text.Parsec.Error

import Frontend.Prelude

type FieldParser a = Parsec String () a


-- * field validation

-- FIXME: Use (Error -> Html) instead of toHtml
-- FIXME: Use red color for error message when displaying them on the form.
fieldValidation :: String -> FieldParser a -> String -> TD.Result (Html ()) a
fieldValidation name parser value =
    either (TD.Error . toHtml . errorString) TD.Success $ parse (parser <* eof) name value
  where
    errorString e = filter (/='\n') $ unwords [sourceName $ errorPos e, ":", errorMsgs $ errorMessages e]
    -- | Parsec uses 'ParseError' which contains a list of 'Message's, which
    -- are displayed if a parse error happens. Also it gives control to the
    -- client code to make their translation of those connectors.
    -- TODO: Translation :)
    errorMsgs = showErrorMessages "or" "unknown" "excepting" "unexpected" "end of input"

validate :: (Monad m) => String -> FieldParser a -> Form (Html ()) m String -> Form (Html ()) m a
validate n p = TD.validate (fieldValidation n p)

validateOptional :: (Monad m) => String -> FieldParser a -> Form (Html ()) m (Maybe String) -> Form (Html ()) m (Maybe a)
validateOptional n p = TD.validateOptional (fieldValidation n p)

inRange :: Int -> Int -> FieldParser Int
inRange mn mx =
    satisfies isBetween (read <$> many1 digit)
    <??> unwords ["Eine Zahl zwischen", show mn, "und", show mx, "."]
  where
    isBetween n = mn <= n && n <= mx


-- * missing things from parsec

infix 0 <??>

-- | Set the given message if the parser fails as an error message, pretend
-- no input is consumed.
(<??>) :: ParsecT s u m a -> String -> ParsecT s u m a
p <??> msg = TP.try p <?> msg

satisfies :: (a -> Bool) -> ParsecT s u m a -> ParsecT s u m a
satisfies predicate parser = do
    x <- parser
    unless (predicate x) $ fail ""
    return x

-- | Try to apply the given parser minimum 'n' and maximum 'n+m' times.
manyNM
    :: forall s u m a t . (Stream s m t)
    => Int -> Int -> ParsecT s u m a -> ParsecT s u m [a]
manyNM n m p = do
    xs <- replicateM n p
    ys <- run m []
    pure $ xs <> ys
  where
    run :: Int -> [a] -> ParsecT s u m [a]
    run 0 xs = return (reverse xs)
    run l xs = optionMaybe (TP.try p) >>= \case
                    Just x -> (run (l-1) (x:xs))
                    Nothing -> run 0 xs
