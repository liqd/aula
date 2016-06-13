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

      -- * common validators
    , inRangeV
    , nonEmptyV
    , maxLengthV
    , DfForm
    , DfTextField
    , dfTextField
    , StringFieldValidator
    , usernameV'
    , usernameV
    , passwordV
    , titleV
    , markdownV
    , emailField

      -- * missing parser combinators
    , (<??>)
    , satisfies
    , manyNM
    )
where

import Prelude hiding ((.))

import Control.Arrow
import Control.Category as Cat

import Text.Digestive as DF
import Text.Email.Validate as Email
import Text.Parsec as TP hiding (Reply(..))
import Text.Parsec.Error

import Frontend.Prelude as Frontend hiding ((<|>))

import qualified Data.Text as ST 


type FieldName = String
type FieldParser a = Parsec String () a


-- * field validation

newtype FieldValidator a b = FieldValidator { unFieldValidator :: a -> DF.Result [ST] b }

type FieldValidator' a = FieldValidator a a

instance Functor (FieldValidator a) where
    fmap g (FieldValidator f) = FieldValidator (fmap g . f)

instance Profunctor FieldValidator where
    dimap g h (FieldValidator f) = FieldValidator (fmap h . f . g)

instance Cat.Category FieldValidator where
    id    = FieldValidator DF.Success
    g . f = FieldValidator (unFieldValidator g <=< unFieldValidator f)

instance Arrow FieldValidator where
    arr f   = FieldValidator (DF.Success . f)
    first f = FieldValidator $ \(x, y) -> case unFieldValidator f x of
                DF.Success z -> DF.Success (z, y)
                DF.Error   e -> DF.Error e

fieldEither :: (a -> Either [ST] b) -> FieldValidator a b
fieldEither fun = FieldValidator $ either DF.Error DF.Success . fun

-- FIXME: Use (Error -> Html) instead of toHtml. (In other words: use typed
-- validation errors instead of strings).
-- FIXME: Use red color for error message when displaying them on the form.
fieldParser
    :: (ConvertibleStrings s String)
    => FieldParser a -> FieldValidator s a
fieldParser parser =
    FieldValidator (either errorString DF.Success . parse (parser <* eof) "" . cs)
  where
    errorString = DF.Error . fmap cs . showErrorMessagesDe . errorMessages

-- | Cloned from "Text.Parsec.Error" for better (and German) errors.
showErrorMessagesDe :: [Message] -> [String]
showErrorMessagesDe [] = ["ungültige Eingabe."]
showErrorMessagesDe msgs = (:[]) . filter (/= '\n') . mconcat . clean $
      [showSysUnExpect, showUnExpect, " (", showExpect, ")", showMessages]
    where
      msgOr         :: String = "oder"
      msgExpecting  :: String = "erwartet:"
      msgUnExpected :: String = "ungültige Eingabe:"
      msgEndOfInput :: String = "zu wenig Input"

      (sysUnExpect,msgs1) = span (SysUnExpect "" ==) msgs
      (unExpect,msgs2)    = span (UnExpect    "" ==) msgs1
      (expect,messages)   = span (Expect      "" ==) msgs2

      showExpect      = showMany msgExpecting expect
      showUnExpect    = showMany msgUnExpected unExpect
      showSysUnExpect | not (null unExpect) ||
                        null sysUnExpect = ""
                      | null firstMsg    = msgUnExpected <> " " <> msgEndOfInput
                      | otherwise        = msgUnExpected <> " " <> firstMsg
          where
              firstMsg  = messageString (head sysUnExpect)

      showMessages      = showMany "" messages

      -- helpers
      showMany p ms = case clean (map messageString ms) of
                            []              -> ""
                            ms' | null p    -> commasOr ms'
                                | otherwise -> p <> " " <> commasOr ms'

      commasOr []       = ""
      commasOr [m]      = m
      commasOr ms       = commaSep (init ms) <> " " <> msgOr <> " " <> last ms

      commaSep          = separate ", " . clean

      separate   _ []     = ""
      separate   _ [m]    = m
      separate sep (m:ms) = m <> sep <> separate sep ms

      clean            :: [String] -> [String]
      clean             = nub . filter (not . null)


validate' :: (Monad m) => FieldName -> FieldValidator a b -> a -> Result (HtmlT m ()) b
validate' n v = errorToHtml . unFieldValidator (addFieldNameToError n v)
  where
    errorToHtml (DF.Success x) = DF.Success x
    errorToHtml (DF.Error x)   = DF.Error $ toHtml `mapM_` x

    addFieldNameToError :: FieldName -> FieldValidator a b -> FieldValidator a b
    addFieldNameToError fieldName (FieldValidator w) = FieldValidator $ \x -> case w x of
        s@(DF.Success _) -> s
        DF.Error es      -> DF.Error $ ((cs fieldName <> ": ") <>) <$> es

validate
    :: Monad m => FieldName -> FieldValidator s a -> Form (Html ()) m s -> Form (Html ()) m a
validate = DF.validate <..> validate'

validateOptional
    :: Monad m
    => FieldName -> FieldValidator s a -> Form (Html ()) m (Maybe s) -> Form (Html ()) m (Maybe a)
validateOptional = DF.validateOptional <..> validate'


-- * simple validators

inRangeV :: (ConvertibleStrings s String) => Int -> Int -> FieldValidator s Int
inRangeV mn mx = fieldParser
    (satisfies isBetween (read <$> many1 digit)
        <??> unwords ["Zahl zwischen", show mn, "und", show mx])
  where
    isBetween n = mn <= n && n <= mx

nonEmptyV :: (Eq m, Monoid m) => FieldValidator' m
nonEmptyV = FieldValidator $ \xs ->
    if xs == mempty
        then DF.Error ["darf nicht leer sein"]
        else DF.Success xs

maxLengthV :: Int -> FieldValidator' ST
maxLengthV mx = FieldValidator $ \xs ->
    if ST.length xs > mx
        then DF.Error ["max." <> cs (show mx) <> " Zeichen"]
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

type StringFieldValidator = forall r s . (ConvertibleStrings r String, ConvertibleStrings String s)
                                         => FieldValidator r s

-- See `usernameV`
usernameV' :: FieldValidator' UserLogin
usernameV' = dimap (view _UserLogin) UserLogin usernameV

-- WARNING: we also apply the validation rules on the login page.
-- As long as we do not change the validation rules this is not a problem.
-- One reason one might want to validate logins everywhere is to avoid potential
-- security issues such as https://labs.spotify.com/2013/06/18/creative-usernames/
-- The issue above does not apply here for various reasons but still we can be cautious.
usernameV :: StringFieldValidator
usernameV = fieldParser (cs <$> manyNM 4 12 letter <??> "4-12 Buchstaben")

passwordV :: StringFieldValidator
passwordV = fieldParser (cs <$> manyNM 4 12 anyChar <??> "4-12 Zeichen")

titleV :: StringFieldValidator
titleV = fieldParser (cs <$> many1 (alphaNum <|> space) <??> "Buchstaben, Ziffern, oder Leerzeichen")

markdownV :: FieldValidator ST Document
markdownV = nonEmptyV >>> fieldEither markdown

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
