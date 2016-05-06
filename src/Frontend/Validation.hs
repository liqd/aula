{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
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
    , fieldParser
    , nonEmpty
    , maxLength
    , DfForm
    , DfTextField
    , dfTextField

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
    , markdown
    )
where

import Prelude hiding ((.))

import Control.Arrow
import Control.Category as Cat
import Data.Text as Text (Text, length)

import Text.Digestive as DF
import Text.Email.Validate as Email
import Text.Parsec as TP hiding (Reply(..))
import Text.Parsec.Error

import Frontend.Prelude as Frontend hiding ((<|>))

type FieldName = String
type FieldParser a = Parsec String () a


-- * field validation

newtype FieldValidator a b = FieldValidator { unFieldValidator :: a -> DF.Result ST b }

instance Functor (FieldValidator a) where
    fmap g (FieldValidator f) = FieldValidator (fmap g . f)

instance Cat.Category FieldValidator where
    id    = FieldValidator DF.Success
    g . f = FieldValidator (unFieldValidator g <=< unFieldValidator f)

instance Arrow FieldValidator where
    arr f   = FieldValidator (DF.Success . f)
    first f = FieldValidator $ \(x,y) -> case unFieldValidator f x of
                DF.Success z -> DF.Success (z,y)
                DF.Error   e -> DF.Error e

-- FIXME: Use (Error -> Html) instead of toHtml. (In other words: use typed
-- validation errors instead of strings).
-- FIXME: Use red color for error message when displaying them on the form.
fieldParser
    :: (ConvertibleStrings s String)
    => FieldParser a -> FieldValidator s a
fieldParser parser =
    FieldValidator (either (DF.Error . cs . errorString) DF.Success . parse (parser <* eof) "" . cs)
  where
    errorString = filter (/= '\n') . errorMsgs . errorMessages
    -- | Parsec uses 'ParseError' which contains a list of 'Message's, which
    -- are displayed if a parse error happens. Also it gives control to the
    -- client code to make their translation of those connectors. The German
    -- translations here are probably not helping to form perfect phrases in
    -- all situations.
    errorMsgs = showErrorMessages "oder" "unbekannt" "erwartet" "unerwartet" "zu kurz"

validate' :: (Monad m) => FieldName -> FieldValidator a b -> a -> Result (HtmlT m ()) b
validate' n v = errorToHtml . unFieldValidator (addFieldNameToError n v)
  where
    errorToHtml (DF.Success x) = DF.Success x
    errorToHtml (DF.Error x)   = DF.Error $ toHtml x

    addFieldNameToError :: FieldName -> FieldValidator a b -> FieldValidator a b
    addFieldNameToError fieldName v = FieldValidator $ \x -> case unFieldValidator v x of
        s@(DF.Success _) -> s
        DF.Error e       -> DF.Error $ cs fieldName <> ": " <> e

validate
    :: Monad m => FieldName -> FieldValidator s a -> Form (Html ()) m s -> Form (Html ()) m a
validate = DF.validate <..> validate'

validateOptional
    :: Monad m
    => FieldName -> FieldValidator s a -> Form (Html ()) m (Maybe s) -> Form (Html ()) m (Maybe a)
validateOptional = DF.validateOptional <..> validate'

inRange :: (ConvertibleStrings s String) => Int -> Int -> FieldValidator s Int
inRange mn mx = fieldParser
    (satisfies isBetween (read <$> many1 digit)
        <??> unwords ["Eine Zahl zwischen", show mn, "und", show mx, "."])
  where
    isBetween n = mn <= n && n <= mx


-- * simple validators

nonEmpty :: (Eq m, Monoid m) => FieldValidator m m
nonEmpty = FieldValidator $ \xs ->
    if xs == mempty
        then DF.Error . fromString $ "darf nicht leer sein"
        else DF.Success xs

maxLength :: Int -> FieldValidator Text Text
maxLength mx = FieldValidator $ \xs ->
    let l = Text.length xs
    in if Text.length xs > mx
        then DF.Error . fromString $
            unwords ["zu lang, Zahl der zusätzlichen Zeichen:", show (l - mx)]
        else DF.Success xs

type DfForm a = forall m. Monad m => DF.Form (Html ()) m a
type DfTextField s = forall a. Getter s a -> Traversal' a ST -> DfForm a

-- Usage:
--    SomeConstructor
--    <$> ("field1" .: field someLens1 _SomePrism1)
--    <*> ("field2" .: field someLens2 _SomePrism2)
--  where
--    field :: DfTextField SomeType
--    field = dfTextField someData
dfTextField :: s -> DfTextField s
dfTextField s l p = s ^. l & p %%~ DF.text . Just


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

type StringFieldValidator = forall r s . (ConvertibleStrings r String, ConvertibleStrings String s)
                                         => FieldValidator r s

username :: StringFieldValidator
username = fieldParser (cs <$> manyNM 4 8 letter <??> "4-12 Buchstaben")

password :: StringFieldValidator
password = fieldParser (cs <$> manyNM 4 8 anyChar <??> "Ungültiges Passwort (muss 4-12 Zeichen lang sein)")

title :: StringFieldValidator
title = fieldParser (cs <$> many1 (alphaNum <|> space))

markdown :: FieldValidator Document Document
markdown = unMarkdown ^>> nonEmpty >>^ Markdown

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
