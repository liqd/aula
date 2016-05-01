{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wall -Werror #-}

module Frontend.Validation
    ( module TP
      -- * field validation
    , FieldName
    , FieldParser
    , Frontend.Validation.validate
    , Frontend.Validation.validateOptional
    , nonEmpty
    , optionalNonEmpty

      -- * missing parser combinators
    , (<??>)
    , inRange
    , manyNM
    , satisfies

      -- * common validators
    , username
    , password
    , title
    , emailField
    , validateMarkdown
    , validateOptionalMarkdown
    )
where

import Text.Digestive as DF
import Text.Email.Validate as Email
import Text.Parsec as TP hiding (Reply(..))
import Text.Parsec.Error

import Frontend.Prelude as Frontend hiding ((<|>))

type FieldName = String
type FieldParser a = Parsec String () a


-- * field validation

-- FIXME: Use (Error -> Html) instead of toHtml. (In other words: use typed
-- validation errors instead of strings).
-- FIXME: Use red color for error message when displaying them on the form.
fieldValidation
    :: (ConvertibleStrings s String)
    => FieldName -> FieldParser a -> s -> DF.Result (Html ()) a
fieldValidation name parser value =
    either (DF.Error . toHtml . errorString) DF.Success $ parse (parser <* eof) name (cs value)
  where
    errorString e = filter (/= '\n') $ unwords [sourceName $ errorPos e, ":", errorMsgs $ errorMessages e]
    -- | Parsec uses 'ParseError' which contains a list of 'Message's, which
    -- are displayed if a parse error happens. Also it gives control to the
    -- client code to make their translation of those connectors. The German
    -- translations here are probably not helping to form perfect phrases in
    -- all situations.
    errorMsgs = showErrorMessages "oder" "unbekannt" "erwartet" "unerwartet" "zu kurz"

validate
    :: (Monad m, ConvertibleStrings s String)
    => FieldName -> FieldParser a -> Form (Html ()) m s -> Form (Html ()) m a
validate n p = DF.validate (fieldValidation n p)

validateOptional
    :: (Monad m, ConvertibleStrings s String)
    => FieldName -> FieldParser a -> Form (Html ()) m (Maybe s) -> Form (Html ()) m (Maybe a)
validateOptional = DF.validateOptional <..> fieldValidation

inRange :: Int -> Int -> FieldParser Int
inRange mn mx =
    satisfies isBetween (read <$> many1 digit)
    <??> unwords ["Eine Zahl zwischen", show mn, "und", show mx, "."]
  where
    isBetween n = mn <= n && n <= mx


-- * simple validators

checkNonEmpty
    :: (Eq m, IsString v, Monoid m)
    => FieldName -> m -> DF.Result v m
checkNonEmpty name xs
   | xs == mempty = DF.Error . fromString $ unwords [name, ":", "darf nicht leer sein"]
   | otherwise    = DF.Success xs

nonEmpty
    :: (Monad m, Monoid v, IsString v, Eq s, Monoid s, ConvertibleStrings s r)
    => FieldName -> Form v m s -> Form v m r
nonEmpty = DF.validate . fmap cs <..> checkNonEmpty

optionalNonEmpty
    :: (Monad m, Monoid v, IsString v)
    => FieldName -> Form v m (Maybe String) -> Form v m (Maybe String)
optionalNonEmpty = DF.validateOptional . checkNonEmpty

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
manyNM n m p = (<>) <$> replicateM n p <*> run m []
  where
    run :: Int -> [a] -> ParsecT s u m [a]
    run 0 xs = pure (reverse xs)
    run l xs = optionMaybe (TP.try p) >>= \case
                    Just x -> (run (l-1) (x:xs))
                    Nothing -> run 0 xs


-- * common validators

type StringFieldParser = forall s . ConvertibleStrings String s => FieldParser s

username :: StringFieldParser
username = cs <$> manyNM 4 8 letter <??> "4-12 Buchstaben"

password :: StringFieldParser
password = cs <$> manyNM 4 8 anyChar <??> "Ungültiges Passwort (muss 4-12 Zeichen lang sein)"

title :: StringFieldParser
title = cs <$> many1 (alphaNum <|> space)

-- FIXME: Use LensLike
validateMarkdown
    :: (Monad m)
    => FieldName -> Form (Html ()) m Document -> Form (Html ()) m Document
validateMarkdown name = fmap Markdown . nonEmpty name . fmap unMarkdown

validateOptionalMarkdown
    :: Monad m
    => FieldName -> Form (Html ()) m (Maybe String) -> Form (Html ()) m (Maybe Document)
validateOptionalMarkdown name = ((Markdown . cs) <$$>) . optionalNonEmpty name

emailField :: FieldName -> Maybe Frontend.EmailAddress -> DfForm (Maybe Frontend.EmailAddress)
emailField name emailValue =
    {-  Since not all texts values are valid email addresses, emailAddress is a @Prism@
        from texts to @EmailAddress@. Here we want to traverse the text of an email address
        thus one needs to reverse this prism. While Prisms cannot be reversed in full
        generality, we could expect a weaker form which also traversals. This would look
        like that:

        email & rev emailAddress %%~ DF.optionalText

        Instead, we have the code below which extracts the text of the email address if
        there is such an email address.  'optionalText' gets a @Maybe ST@, finally the
        result of 'optionalText' is processed with a pure function from @Maybe ST@ to
        @Maybe EmailAddress@ where only a valid text representation of an email gets
        mapped to @Just@  of an @EmailAddress@.
    -}
    DF.validateOptional
        checkEmail
        (DF.optionalText (emailValue ^? _Just . re Frontend.emailAddress))
  where
    checkEmail value = case Email.emailAddress (cs value) of
        Nothing -> DF.Error . fromString $ unwords [name, ":", "Invalid email address"]
        Just e  -> DF.Success $ InternalEmailAddress e
